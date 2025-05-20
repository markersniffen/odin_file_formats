package iso_bmff
/*
	This is free and unencumbered software released into the public domain.

	Anyone is free to copy, modify, publish, use, compile, sell, or
	distribute this software, either in source code form or as a compiled
	binary, for any purpose, commercial or non-commercial, and by any
	means.

	In jurisdictions that recognize copyright laws, the author or authors
	of this software dedicate any and all copyright interest in the
	software to the public domain. We make this dedication for the benefit
	of the public at large and to the detriment of our heirs and
	successors. We intend this dedication to be an overt act of
	relinquishment in perpetuity of all present and future rights to this
	software under copyright law.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
	IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
	OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
	ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
	OTHER DEALINGS IN THE SOFTWARE.

	For more information, please refer to <https://unlicense.org/>


	A from-scratch implementation of ISO base media file format (ISOM),
	as specified in ISO/IEC 14496-12, Fifth edition 2015-12-15.
	The identical text is available as ISO/IEC 15444-12 (JPEG 2000, Part 12).

	See: https://www.iso.org/standard/68960.html and https://www.loc.gov/preservation/digital/formats/fdd/fdd000079.shtml

	This file contains the base media format parser.
*/

import os "core:os/os2"
@(require) import "core:fmt"
import "../common"
import "core:mem"

DEBUG         :: #config(BMFF_DEBUG, false)
DEBUG_VERBOSE :: DEBUG && #config(BMFF_DEBUG_VERBOSE, false)

intern_payload :: proc(box: ^BMFF_Box, payload: $T, loc := #caller_location) {
	when T == []u8 {
		box.payload = [dynamic]u8{}
		append(&box.payload.([dynamic]u8), ..payload)

	} else {
		fmt.panicf("Unhandled: intern_payload(%v), called from %v\n", typeid_of(T), loc)
	}
}

free_atom :: proc(atom: ^BMFF_Box, allocator := context.allocator) {
	atom := atom

	for atom != nil {
		when DEBUG_VERBOSE {
			fmt.printfln("Freeing '%v' (0x%08x).", _string(atom.type), int(atom.type))
		}

		if atom.payload != nil {
			switch v in atom.payload {

			case [dynamic]u8: delete(v)
			case ELST_V0:     delete(v.entries)
			case ELST_V1:     delete(v.entries)
			case FTYP:        delete(v.compatible)
			case HDLR:        delete(v.name)
			case URL:         delete(v.location)
			// case URN:         // delete(v.location); delete(v.name)
			case AVCDecoderConfigurationRecord:
				delete(v.SequenceParameterSets)
				delete(v.PictureParameterSets)
				delete(v.SequenceParameterSetExt)
			case STTS:
				// delete(v.entries) // do I need these?
			case STSZ:
				// delete(v.entry_sizes) // do I need these?
			case STSS:

			// iTunes metadata types
			case iTunes_Metadata:
				switch w in v.data {
				case cstring:     delete(w)
				case [dynamic]u8: delete(w)
				case iTunes_Track, iTunes_Disk: // Nothing to free.
				}

			case Chapter_List:
				for chapter in v.chapters {
					delete(chapter.title)
				}
				delete(v.chapters)

			// These are just structs with no allocated items to free.
			case MDHD_V0, MDHD_V1:
			case MVHD_V0, MVHD_V1:
			case TKHD_V0, TKHD_V1:
			case VMHD:
			case DREF:
			case STSD, Visual_Sample_Entry, STSC, STCO:
			case iTunes_Track, iTunes_Disk:
			case: fmt.panicf("free_atom: Unhandled payload type: %v\n", v)
			}
		}

		if atom.first_child != nil {
			free_atom(atom.first_child)
		}

		ptr_to_free := atom
		atom = atom.next
		free(ptr_to_free)
	}
}

parse_itunes_metadata :: proc(f: ^BMFF_File) -> (err: Error) {
	context.allocator = f.allocator

	assert(f.itunes_metadata != nil)
	when DEBUG {
		fmt.println("\nCalling specialized iTunes metadata parser...")
		defer fmt.println("Back from specialized iTunes metadata parser...\n")
	}

	fd := f.handle

	h:            BMFF_Box_Header
	box:          ^BMFF_Box
	prev:         ^BMFF_Box = f.itunes_metadata
	parent:       ^BMFF_Box = f.itunes_metadata

	loop: for {
		// Peek at header and check if this would put us past the end of the iTunes metadata.
		h = read_box_header(fd=fd, read=false) or_return
		if h.offset >= f.root.end || h.offset > f.itunes_metadata.end {
			// Done.
			return
		}

		// Now read it for real.
		h = read_box_header(fd=fd, read=true) or_return

		// Create box and set type, size, parent, etc.
		#partial switch h.type {
		case .Data:
			if parent == f.itunes_metadata {
				// Fold data into parent.
				metadata := iTunes_Metadata{}

				metadata._ilst_data = common.read_data(fd, _ILST_DATA) or_return

				payload := common.read_slice(fd, h.payload_size - size_of(_ILST_DATA), f.allocator) or_return

				#partial switch metadata.type {
				case .Text:
					metadata.data = cstring(raw_data(payload))

				case: // Binary, JPEG, PNG, ...
					#partial switch prev.type {
					case .iTunes_Track:
						if len(payload) != size_of(iTunes_Track) { return .Wrong_File_Format }
						data := (^iTunes_Track)(raw_data(payload))^
						metadata.data = data
						delete(payload)

					case .iTunes_Disk:
						if len(payload) != size_of(iTunes_Disk) { return .Wrong_File_Format }
						data := (^iTunes_Disk)(raw_data(payload))^
						metadata.data = data
						delete(payload)

					case:
						metadata.data = [dynamic]u8{}
						append(&metadata.data.([dynamic]u8), ..payload)
						delete(payload)
					}
				}

				box.payload = metadata

				prev = box
				continue loop
			}

			parent = prev

		case .iTunes_Mean:
			parent = prev
		case:
			parent = f.itunes_metadata
		}

		box = new(BMFF_Box)
		box.header = h
		box.parent = parent

		// Chain it.
		if parent.first_child == nil {
			// We're either our parent's first child...
			parent.first_child = box
		} else {
			// Or we walk our siblings until its next pointer is nil.
			sibling: ^BMFF_Box
			for sibling = parent.first_child; sibling.next != nil; sibling = sibling.next {}
			sibling.next = box
		}

		when DEBUG {
			level := 1 if box.parent == f.itunes_metadata else 2
			print_box_header(box, level)
		}

		#partial switch box.type {
		case .Data:
			/*
				Apple iTunes mdir metadata tag.
				Found as a child of the various tags under: `moov.udta.meta.ilst`
			*/
			skip := true
			if f.itunes_metadata != nil {
				// We parse if we've previously located the property bag.
				if parent.parent == f.itunes_metadata {
					skip = false
					payload := common.read_slice(fd, box.payload_size) or_return
					intern_payload(box, payload)
				}
			}

			if skip {
				skip_box(fd, box) or_return
			}

		case .iTunes_Extended:
			payload := common.read_slice(fd, box.payload_size) or_return
			intern_payload(box, payload)
		}

		prev = box
	}
	return
}

parse :: proc(f: ^BMFF_File, parse_metadata := true) -> (err: Error) {
	context.allocator = f.allocator

	when DEBUG {
		fmt.println("\nParsing...")
		defer fmt.println("\nBack from parsing...")
	}

	fd := f.handle

	h:            BMFF_Box_Header
	box:          ^BMFF_Box
	prev:         ^BMFF_Box = f.root
	parent:       ^BMFF_Box = f.root

	// Most files start with an 'ftyp' atom.
	h = read_box_header(fd=fd, read=false) or_return
	if h.type != .File_Type {
		// NOTE(Jeroen): Files with no file‐type box should be read as if they contained an FTYP box with
		//               Major_brand='mp41', minor_version=0, and the single compatible brand `mp41`.
		box = new(BMFF_Box)
		box.size           = 0
		box.type           = .File_Type
		box.parent         = f.root
		f.root.first_child = box

		ftyp := FTYP{
			header = { .mp41, 0, },
		}
		append(&ftyp.compatible, FourCC.mp41)
		intern_payload(box, ftyp)
	}

	loop: for {
		h = read_box_header(fd=fd, read=true) or_return
		if h.offset >= f.root.size {
			// Done
			return
		}

		// Find the parent by what byte range of the file we're at.
		parent = prev
		for {
			if h.offset >= parent.end {
				// Parent can't contain this box. Let's look at its parent.
				when DEBUG_VERBOSE {
					fmt.printf("\t[%v] ends past ",      _string(h.type))
					fmt.printf("[%v] end, checking if ", _string(parent.type))
					fmt.printf("[%v] is our parent.\n",  _string(parent.parent.type))
				}
				parent = parent.parent
			} else {
				// Box fits within this parent.
				break
			}
		}

		// Create box and set type, size, parent, etc.
		box = new(BMFF_Box)
		box.header = h
		box.parent = parent

		// Chain it.
		if parent.first_child == nil {
			// We're either our parent's first child...
			parent.first_child = box
		} else {
			// Or we walk our siblings until its next pointer is `nil`.
			sibling: ^BMFF_Box
			for sibling = parent.first_child; sibling.next != nil; sibling = sibling.next {}
			sibling.next = box
		}

		if box.end > f.root.size {
			when DEBUG {
				fmt.printf("\t[%v] ended early, expected to end at %v.\n", _string(h.type), box.end)
			}
			return .File_Ended_Early
		}

		when DEBUG {
			level := 0
			for p := box.parent; p != f.root; p = p.parent { level += 1 }
			print_box_header(box, level)
		}

		#partial switch h.type {
		case .File_Type:
			// `ftyp` must always be the first child and we can't have two nodes of this type.
			if f.root.first_child != box {
				return .FTYP_Duplicated
			}
			f.ftyp = box

			if box.payload_size % size_of(FourCC) != 0 || box.payload_size < size_of(_FTYP) {
				/*
					Remaining:
					- Major Brand: FourCC
					- Minor Brand: BCD4
					- ..FourCC

					All have the same size, so the remaining length of this box should cleanly divide by `size_of(FourCC)`.
				*/
				return .FTYP_Invalid_Size
			}

			_ftyp := common.read_data(fd, _FTYP) or_return
			ftyp  := FTYP{ header = _ftyp, }

			compat_count := (box.payload_size - size_of(_FTYP)) / size_of(FourCC)
			for _ in 0..<compat_count {
				compat := common.read_data(fd, FourCC) or_return
				append(&ftyp.compatible, compat)
			}
			box.payload = ftyp

		case .Movie:
			f.moov = box

		case .Movie_Header:
			f.mvhd = box

			version := common.peek_u8(fd) or_return
			if version > 1 { return .MVHD_Unknown_Version }

			switch version {
			case 0:
				if box.payload_size != size_of(MVHD_V0) { return .MVHD_Invalid_Size }
				box.payload = common.read_data(fd, MVHD_V0) or_return

				f.time_scale = box.payload.(MVHD_V0).time_scale
			case 1:
				if box.payload_size != size_of(MVHD_V1) { return .MVHD_Invalid_Size }
				box.payload = common.read_data(fd, MVHD_V1) or_return

				f.time_scale = box.payload.(MVHD_V1).time_scale
			case:
				unreachable()
			}

		case .Track:

		case .Track_Header:
			version := common.peek_u8(fd) or_return
			if version > 1 { return .TKHD_Unknown_Version }

			switch version {
			case 0:
				if box.payload_size != size_of(TKHD_V0) { return .TKHD_Invalid_Size }
				box.payload = common.read_data(fd, TKHD_V0) or_return
			case 1:
				if box.payload_size != size_of(TKHD_V1) { return .TKHD_Invalid_Size }
				box.payload = common.read_data(fd, TKHD_V1) or_return
			case:
				unreachable()
			}

		case .Edit:

		case .Edit_List:
			version := common.peek_u8(fd) or_return
			if version > 1 { return .ELST_Unknown_Version }

			elst_hdr := common.read_data(fd, _ELST) or_return

			switch version {
			case 0:
				if box.payload_size != i64(size_of(_ELST)) + i64(elst_hdr.entry_count) * size_of(ELST_Entry_V0) { return .ELST_Invalid_Size }

				elst := ELST_V0{ header = elst_hdr }
				for _ in 0..<elst_hdr.entry_count {
					entry := common.read_data(fd, ELST_Entry(u32be)) or_return
					append(&elst.entries, entry)
				}
				box.payload = elst
			case 1:
				if box.payload_size != i64(size_of(_ELST)) + i64(elst_hdr.entry_count) * size_of(ELST_Entry_V1) { return .ELST_Invalid_Size }

				elst := ELST_V1{ header = elst_hdr }
				for _ in 0..<elst_hdr.entry_count {
					entry := common.read_data(fd, ELST_Entry(u64be)) or_return
					append(&elst.entries, entry)
				}
				box.payload = elst
			case:
				unreachable()
			}

		case .Media:

		case .Media_Header: // mdhd
			version := common.peek_u8(fd) or_return
			if version > 1 { return .MDHD_Unknown_Version }

			switch version {
			case 0:
				if box.payload_size != size_of(MDHD_V0) { return .MDHD_Invalid_Size }
				box.payload = common.read_data(fd, MDHD_V0) or_return
			case 1:
				if box.payload_size != size_of(MDHD_V1) { return .MDHD_Invalid_Size }
				box.payload = common.read_data(fd, MDHD_V1) or_return
			case:
				unreachable()
			}

		case .Handler_Reference:
			/*
				ISO 14496-12-2015, section 8.4.3.1:
				`hdlr` may be contained in a `mdia` or `meta` box.
			*/
			if !(box.parent.type == .Media || box.parent.type == .Meta) {
				skip_box(fd, box) or_return
				break
				// return .HDLR_Unexpected_Parent
			}
			if box.payload_size < size_of(_HDLR) { return .HDLR_Invalid_Size }

			_hdlr := common.read_data(fd, _HDLR) or_return
			hdlr := HDLR { _hdlr = _hdlr }

			name_bytes := common.read_slice(fd, box.payload_size - size_of(_HDLR), f.allocator) or_return
			hdlr.name   = cstring(raw_data(name_bytes))
			box.payload = hdlr

		case .Media_Information: // minf - just container

		case .Video_Media_Header: // vmhd
			vmhd := common.read_data(fd, VMHD) or_return
			box.payload = vmhd

		case .Data_Information: // dinf

		case .Data_Reference: // dref
			dref := common.read_data(fd, DREF) or_return
			box.payload = dref

		case .Data_Ref_URL:
			_url := common.read_data(fd, _URL) or_return
			url := URL { _url = _url }
			location_bytes := common.read_slice(fd, box.payload_size - size_of(_URL), f.allocator) or_return
			url.location = cstring(raw_data(location_bytes))
			box.payload = url

		// case .Data_Ref_URN: .. not implemented yet

		case .Sample_Table: // stbl - just container

		case .Sample_Description: // stsd
			stsd := common.read_data(fd, STSD) or_return


		case .avc1: //, .mp41, .mp42, .mp71:
			visual_sample_entry := common.read_data(fd, Visual_Sample_Entry) or_return
			box.payload = visual_sample_entry
			fmt.println(string(visual_sample_entry.compressorname[:]), visual_sample_entry)

		case .avcC:
			avcC_bytes := common.read_slice(fd, box.payload_size, f.allocator) or_return
			config_record: AVCDecoderConfigurationRecord
			parse_AVCDecoderConfigurationRecord(avcC_bytes, &config_record)
			box.payload = config_record
			
		case .Time_To_Sample: // stts
			_stts := common.read_data(fd, _STTS) or_return
			entries_bytes := common.read_slice(fd, box.payload_size - size_of(_STTS), f.allocator) or_return
			stts: STTS= { _stts = _stts }
			stts.entries = mem.slice_data_cast([]STTS_Entry, entries_bytes)
			box.payload = stts

		case .Sample_To_Chunk: // stsc
			_stsc := common.read_data(fd, _STSC) or_return
			entries_bytes := common.read_slice(fd, box.payload_size - size_of(_STSC), f.allocator) or_return
			stsc: STSC = { _stsc = _stsc }
			stsc.entries = mem.slice_data_cast([]STSC_Entry, entries_bytes)
			box.payload = stsc
			for e in stsc.entries {
				fmt.println(">>", e)
			}

		case .Sample_Size: // stsz
			_stsz := common.read_data(fd, _STSZ) or_return
			samples_bytes := common.read_slice(fd, box.payload_size - size_of(_STSZ), f.allocator) or_return
			stsz: STSZ = { _stsz = _stsz }
			stsz.sample_sizes = mem.slice_data_cast([]u32be, samples_bytes)
			box.payload = stsz

		case .Chunk_Offset: // stco
			_stco := common.read_data(fd, _STCO) or_return
			entries_bytes := common.read_slice(fd, box.payload_size - size_of(_STCO), f.allocator) or_return
			stco: STCO = { _stco = _stco }
			stco.chunk_offsets = mem.slice_data_cast([]u32be, entries_bytes)
			box.payload = stco

		case .Sync_Sample_Table: // stss
			_stss := common.read_data(fd, _STSS) or_return
			entries_bytes := common.read_slice(fd, box.payload_size - size_of(_STCO), f.allocator) or_return
			stss: STSS = { _stss = _stss }
			stss.sample_numbers = mem.slice_data_cast([]u32be, entries_bytes)
			box.payload = stss
			fmt.println(stss)

		case .User_Data: // udta
			if !(box.parent.type == .Movie || box.parent.type == .Movie_Fragment || box.parent.type == .Track || box.parent.type == .Track_Fragment) {
				return .Wrong_File_Format
			}

		case .Meta:
			payload := common.read_slice(fd, size_of(META)) or_return
			intern_payload(box, payload)

		case .iTunes_Metadata:
			f.itunes_metadata = box

			if parse_metadata {
				// Apple Metadata. Not part of the ISO standard, but we'll handle it anyway.
				parse_itunes_metadata(f) or_return
			} else {
				skip_box(fd, box) or_return
			}

		case .Name:
			if parent.type == .User_Data {
				payload := common.read_slice(fd, box.payload_size) or_return
				intern_payload(box, payload)

			} else {
				skip_box(fd, box) or_return
			}

		case .Chapter_List:
			if parent.type == .User_Data {
				vf := common.read_data(fd, Version_and_Flags) or_return
				if vf.version > 1 { return .CHPL_Unknown_Version }

				entry_count: u32be

				if vf.version == 0 {
					short_count := common.read_u8(fd) or_return
					entry_count  = u32be(short_count)
				} else {
					common.read_u8(fd) or_return // Skip reserved field
					entry_count  = common.read_data(fd, u32be) or_return
				}

				chapter_list := Chapter_List{}
				for i := u32be(0); i < entry_count; i += 1 {
					_entry      := common.read_data(fd, _Chapter_Entry) or_return
					title_bytes := common.read_slice(fd, _entry.title_size, f.allocator) or_return
					entry := Chapter_Entry{
						timestamp = _entry.timestamp,
						title     = string(title_bytes),
					}
					append(&chapter_list.chapters, entry)
				}
				box.payload = chapter_list

				// Read cursor should be one past the end of the expected box end.
				cur_pos := common.get_pos(fd) or_return
				if !(cur_pos == box.end + 1) { return .CHPL_Invalid_Size }
			} else {
				skip_box(fd, box) or_return
			}

		case .Media_Data:
			f.mdat = box
			skip_box(fd, box) or_return

		// Boxes we don't (want to or can yet) parse, we skip.
		case:
			if box.end >= i64(f.root.size) { break loop }
			skip_box(fd, box) or_return
			when DEBUG_VERBOSE {
				fmt.printf("[SKIP]", box)
			}
		}

		prev = box
	}
	return
}

skip_box :: proc(fd: ^os.File, box: ^BMFF_Box) -> (err: Error) {
	assert(box != nil)
	common.set_pos(fd, box.end + 1) or_return
	return
}

read_box_header :: #force_inline proc(fd: ^os.File, read := true) -> (header: BMFF_Box_Header, err: Error) {
	h: _BMFF_Box_Header

	header.offset         = common.get_pos(fd) or_return
	header.payload_offset = header.offset

	/*
		Read the basic box header.
	*/
	h = common.read_data(fd, _BMFF_Box_Header) or_return

	header.size            = i64(h.size)
	header.type            = h.type
	header.payload_offset += size_of(_BMFF_Box_Header)

	if header.size == 1 {
		// This atom has a 64-bit size.
		hsize := common.read_data(fd, u64be) or_return
		header.payload_offset += size_of(u64be)
		header.size            = i64(hsize)

	} else if header.size == 0 {
		// This atom runs until the end of the file.
		file_size := os.file_size(fd) or_return
		header.size = file_size - header.offset
	}
	// fmt.println(">>", header.type, header.offset, header.size, header.end)

	header.end = header.offset + header.size - 1

	if header.type == .UUID {
		// Read extended type.
		header.uuid = common.read_data(fd, UUID) or_return
		header.payload_offset += size_of(UUID)
	}

	header.payload_size = header.end - header.payload_offset + 1

	when DEBUG_VERBOSE {
		verb := "read_box_header" if read else "peek_box_header"
		if header.type == .uuid {
			fmt.printf("[%v] 'uuid' (%v) Size: %v\n", verb, _string(header.uuid), header.size)
		} else {
			fmt.printf("[%v] '%v' (0x%08x) Size: %v\n", verb, _string(FourCC(header.type)), int(header.type), header.size)
		}
	}

	// Rewind if peeking.
	if !read {
		common.set_pos(fd, header.offset) or_return
	}

	return
}

open_from_filename :: proc(filename: string, allocator := context.allocator) -> (file: ^BMFF_File, err: Error) {
	context.allocator = allocator

	fd, os_err := os.open(filename, os.O_RDONLY, 0)
	if os_err == nil {
		return open_from_handle(fd, allocator)
	}
	return {}, .File_Not_Found
}

open_from_handle :: proc(handle: ^os.File, allocator := context.allocator) -> (file: ^BMFF_File, err: Error) {
	context.allocator = allocator

	file = new(BMFF_File, allocator)
	file.allocator = allocator
	file.handle    = handle
	file.file_info = os.fstat(handle, allocator) or_return

	if file.file_info.size == 0 {
		close(file)
		return file, .File_Empty
	}

	file.root                = new(BMFF_Box, allocator)
	file.root.offset         = 0
	file.root.size           = file.file_info.size
	file.root.end            = file.file_info.size - 1
	file.root.type           = .Root
	file.root.payload_offset = 0
	file.root.payload_size   = file.file_info.size

	return
}

open :: proc { open_from_filename, open_from_handle, }

close :: proc(file: ^BMFF_File) {
	if file == nil {
		return
	}

	context.allocator = file.allocator

	when DEBUG_VERBOSE {
		fmt.println("\n-=-=-=-=-=-=- CLEANING UP -=-=-=-=-=-=-")
	}

	os.file_info_delete(file.file_info, file.allocator)
	if file.handle != nil {
		os.close(file.handle)
	}
	if file.root != nil {
		free_atom(file.root, file.allocator)
	}
	free(file)

	when DEBUG_VERBOSE {
		fmt.println("-=-=-=-=-=-=- CLEANED UP -=-=-=-=-=-=-")
	}
}