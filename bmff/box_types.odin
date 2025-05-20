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

	This file contains box type definitions.
*/
import os "core:os/os2"
import "core:mem"

/*
	On-disk structures are prefixed with a `_`.
	The parsed versions lack this prefix. If they're the same, there's no prefixed version.
*/

_BMFF_Box_Header :: struct {
	size: u32be,
	type: FourCC,
}
#assert(size_of(_BMFF_Box_Header) == 8)

BMFF_Box_Header :: struct {
	/*
		Box file offset, and size including header.
	*/
	offset:         i64,
	size:           i64,
	end:            i64,

	payload_offset: i64,
	payload_size:   i64,

	type:           FourCC,
	uuid:           UUID,
}

Payload_Types :: union {
	[dynamic]u8,
	ELST_V0, ELST_V1,
	FTYP,
	HDLR,
	MVHD_V0, MVHD_V1,
	MDHD_V0, MDHD_V1,
	TKHD_V0, TKHD_V1,

	Chapter_List,
	iTunes_Metadata,
	iTunes_Track,
	iTunes_Disk,

	VMHD,
	DREF,
	URL,
	// URN,

	STSD, Visual_Sample_Entry,
	STTS,
	STSC,
	STSZ,
	STCO,
	STSS,

	AVCDecoderConfigurationRecord,
}

BMFF_Box :: struct {
	using header: BMFF_Box_Header,

	parent:         ^BMFF_Box,
	next:           ^BMFF_Box,
	first_child:    ^BMFF_Box,

	// Payload can be empty
	payload:        Payload_Types,
}

BMFF_File :: struct {
	// Root atom
	root: ^BMFF_Box,

	// Important atoms
	ftyp: ^BMFF_Box,
	moov: ^BMFF_Box,
	mvhd: ^BMFF_Box,
	mdat: ^BMFF_Box,

	/*
		Apple Metadata is not specified in ISO 14496-12-2015.
		Nevertheless, we add support for it.

		If `moov.udta.meta.hdlr` == `mdir/appl`, then `itunes_metadata` is set to `moov.udta.meta.ilst`
	*/
	itunes_metadata: ^BMFF_Box,

	// Useful file members
	time_scale: u32be,

	// Implementation
	file_info: os.File_Info,
	handle:    ^os.File,
	allocator: mem.Allocator,
}

// Files that don't start with 'ftyp' have this synthetic one.
_FTYP :: struct {
	brand:   FourCC,
	version: BCD4,
}

FTYP :: struct {
	using header: _FTYP,
	compatible:   [dynamic]FourCC,
}

Header_Flag_3 :: enum u8 {
	track_enabled              = 0,
	track_in_movie             = 1,
	track_size_is_aspect_ratio = 2,
}
Header_Flags_3 :: bit_set[Header_Flag_3; u8]

Version_and_Flags :: struct #packed {
	version: u8,
	flag:    [2]u8,
	flags_3: Header_Flags_3,
}

Times :: struct($T: typeid) #packed {
	creation_time:     T,
	modification_time: T,
}

MVHD :: struct($T: typeid) #packed {
	using _vf:         Version_and_Flags,
	using _times:      Times(T),
	time_scale:        u32be,
	duration:          T,
	preferred_rate:    Fixed_16_16,
	preferred_volume:  Fixed_8_8,
	_reserved:         [10]u8,
	view_matrix:       View_Matrix,
	predefined:        MVHD_Predefined,
	next_track_id:     u32be,
}
MVHD_V0 :: distinct MVHD(u32be)
MVHD_V1 :: distinct MVHD(u64be)
#assert(size_of(MVHD_V0) == 100 && size_of(MVHD_V1) == 112)
 
TKHD :: struct($T: typeid) #packed {
	using _vf:         Version_and_Flags,
	using _times:      Times(T),
	track_id:          u32be,
	_reserved_1:       u32be,
	duration:          T,

	_reserved_2:       [2]u32be,
	layer:             i16be,
	alternate_group:   i16be,
	volume:            Fixed_8_8,
	reserved_3:        u16be,
	view_matrix:       View_Matrix,
	width:             Fixed_16_16,
	height:            Fixed_16_16,
}
TKHD_V0 :: distinct TKHD(u32be)
TKHD_V1 :: distinct TKHD(u64be)
#assert(size_of(TKHD_V0) == 84 && size_of(TKHD_V1) == 96)

_ELST :: struct #packed {
	using _vf:         Version_and_Flags,
	entry_count:       u32be,
}
#assert(size_of(_ELST) == 8)

ELST_Entry :: struct($T: typeid) #packed {
	segment_duration:  T,
	media_time:        i32be,
	media_rate:        Rational_16_16,
}
ELST_Entry_V0 :: distinct ELST_Entry(u32be)
ELST_Entry_V1 :: distinct ELST_Entry(u64be)
#assert(size_of(ELST_Entry_V0) == 12 && size_of(ELST_Entry_V1) == 16)

ELST :: struct($T: typeid) #packed {
	using header:      _ELST,
	entries:           [dynamic]ELST_Entry(T),
}
ELST_V0 :: distinct ELST(u32be)
ELST_V1 :: distinct ELST(u64be)

MDHD :: struct($T: typeid) #packed {
	using _vf:         Version_and_Flags,
	using _times:      Times(T),
	time_scale:        u32be,
	duration:          T,
	language:          ISO_639_2, // ISO-639-2/T language code
	quality:           u16be,
}
MDHD_V0 :: distinct MDHD(u32be)
MDHD_V1 :: distinct MDHD(u64be)
#assert(size_of(MDHD_V0) == 24 && size_of(MDHD_V1) == 36)

_HDLR :: struct #packed {
	using _vf:              Version_and_Flags,
	component_type:         FourCC,
	component_subtype:      FourCC,
	component_manufacturer: FourCC,
	component_flags:        u32be,
	reserved:               u32be,
}
#assert(size_of(_HDLR) == 24)

HDLR :: struct #packed {
	using _hdlr:            _HDLR,
	name:                   cstring,
}

META :: struct #packed {
	using vf:               Version_and_Flags,
}
#assert(size_of(META) == 4)

ILST_DATA_Type :: enum u32be {
	Binary =  0, // e.g. moov.udta.meta.ilst.trkn.data
	Text   =  1,

	JPEG   = 13, //      moov.udta.meta.ilst.covr.data
	PNG    = 14, //      moov.udta.meta.ilst.covr.data
}

/*
	Apple iTunes mdir metadata tag.
	Found as a child of the various tags under:
		`moov.udta.meta.ilst`
*/
_ILST_DATA :: struct {
	type:    ILST_DATA_Type,
	subtype: u32be,
}
#assert(size_of(_ILST_DATA) == 8)

iTunes_Metadata :: struct {
	using _ilst_data:       _ILST_DATA,

	data: union {
		[dynamic]u8, // Binary, JPEG, PNG and unknown sub-types
		cstring,     // Text
		iTunes_Track,
		iTunes_Disk,
	},
}

iTunes_Track :: struct #packed {
	_reserved:  u16be,
	current:    u16be,
	disk_total: u16be,
	set_total:  u16be,
}

iTunes_Disk  :: struct #packed {
	_reserved: u16be,
	current:   u16be,
	total:     u16be,
}

/*
	Chapter list, orinally from the F4V format.
	Not part of original ISO spec.
*/
_Chapter_Entry :: struct #packed {
	timestamp:   u64be,
	title_size:  u8,
}

Chapter_Entry :: struct #packed {
	timestamp:   u64be,
	title:       string,
}

Chapter_List :: struct {
	using _vf:   Version_and_Flags,
	_reserved:   u8,
	chapters:    [dynamic]Chapter_Entry,
}



// VIDEO 
VMHD :: struct #packed {
	using _vf:         Version_and_Flags,
	graphicsmode:      u16be,
	opcolor:           [3]u16be,
}

DREF :: struct #packed {
	using _vf:         Version_and_Flags,
	entry_count:       u32be,
} 

_URL :: struct #packed {
	using _vf:         Version_and_Flags,
}

URL :: struct #packed {
	using _url: _URL,
	location: cstring,
}

URN :: struct #packed {
	using _vf:         Version_and_Flags,
	location:          cstring,
	name:              cstring,
}

Sample_Entry :: struct #packed {
	reserved:             [6]u8,
	data_reference_index: u16be,  
}

Visual_Sample_Entry :: struct #packed {
	using sample_entry: Sample_Entry,
	pre_defined:          u16be, //	unsigned int(16) pre_defined = 0;
	reserved2:            u16be, //	const unsigned int(16) reserved = 0;
	pre_defined2:         [3]u32be, //	unsigned int(32)[3] pre_defined = 0;
	width:                u16be, //	unsigned int(16) width;
	height:               u16be, //	unsigned int(16) height;
	horizresolution:      u32be, //	template unsigned int(32) horizresolution = 0x00480000; // 72 dpi
	vertresolution:       u32be, //	template unsigned int(32) vertresolution = 0x00480000; // 72 dpi
	reserved3:            u32be, //	const unsigned int(32) reserved = 0;
	frame_count:          u16be, //	template unsigned int(16) frame_count = 1;
	compressorname:       [32]u8, //	string[32] compressorname;
	depth:                u16be, //	template unsigned int(16) depth = 0x0018;
	pre_defined3:         i16be, //	int(16) pre_defined = -1;
} 

STSD :: struct #packed {
	using _vf:         Version_and_Flags,
	entry_count:       u32be,
}

_STTS :: struct #packed {
	using _vf:         Version_and_Flags,
	entry_count:       u32be,
}

STTS_Entry :: struct #packed {
	sample_count:       u32be,
	sample_delta:       u32be,
}

STTS :: struct #packed {
	using _stts:        _STTS,
	entries:            []STTS_Entry
}

_STSC :: struct #packed {
	using _vf:         Version_and_Flags,
	entry_count:       u32be,
}

STSC_Entry :: struct #packed {
	first_chunk:              u32be,
	samples_per_chunk:        u32be,
	sample_description_index: u32be,
}

STSC :: struct #packed {
	using _stsc:        _STSC,
	entries:            []STSC_Entry
}

_STSZ :: struct #packed {
	using _vf:         Version_and_Flags,
	sample_size:       u32be,
	sample_count:      u32be,
}

STSZ :: struct #packed {
	using _stsz:        _STSZ,
	sample_sizes:        []u32be,
}

_STCO :: struct #packed {
	using _vf:         Version_and_Flags,
	entry_count:       u32be,
}

STCO :: struct #packed {
	using _stco:        _STCO,
	chunk_offsets:      []u32be,
}

_CO64 :: struct #packed {
	using _vf:         Version_and_Flags,
	entry_count:       u32be,
}

CO64 :: struct #packed {
	using _co64:        _CO64,
	chunk_offsets:      []u64be,
}

_STSS :: struct #packed {
	using _vf:         Version_and_Flags,
	entry_count:       u32be,
}

STSS :: struct #packed {
	using _stss:        _STSS,
	sample_numbers:     []u32be,
}

// ISO_IEC_14496-15 (AVC format)

// avcC
AVCDecoderConfigurationRecord :: struct #packed {
    configurationVersion: u8, // 1 byte: always 1
    AVCProfileIndication: u8, // 1 byte: profile ID, e.g., 66 (Baseline), 77 (Main), 100 (High)
		profile_compatibility: u8, // 1 byte: profile constraints (flags bitmask)
		AVCLevelIndication: u8, // 1 byte: level indication, e.g., 30 = Level 3.0

    lengthSizeMinusOne: u8,

    numOfSequenceParameterSets: u8,
		SequenceParameterSets: []ParameterSet,
	
    numOfPictureParameterSets: u8,
    PictureParameterSets: []ParameterSet,

		chroma_format:                u8,
		bit_depth_luma_minus8:        u8,
		bit_depth_chroma_minus8:      u8,
		numOfSequenceParameterSetExt: u8,
		SequenceParameterSetExt:      []ParameterSet,
}

ParameterSet :: struct {
	length: u16be,
	data:   []u8,
}

parse_AVCDecoderConfigurationRecord :: proc(data:[]u8, record:^AVCDecoderConfigurationRecord) {
	record.configurationVersion = data[0]
	record.AVCProfileIndication = data[1]
	record.profile_compatibility = data[2]
	record.AVCLevelIndication  = data[3]
	record.lengthSizeMinusOne = (data[4] & 0b00000011) // TODO: maybe add + 1 and change the name?

	record.numOfSequenceParameterSets = data[5] & 0b00011111
	record.SequenceParameterSets = make([]ParameterSet, record.numOfSequenceParameterSets)
	pos := 6
	for i in 0..<record.numOfSequenceParameterSets {
		sps := &record.SequenceParameterSets[i]
		sps.length = ((^u16be)(&data[pos]))^
		pos += size_of(u16be)
		sps.data = make([]u8, sps.length)
		for &val in sps.data {
			val = data[pos]
			pos += 1
		}
	}

	record.numOfPictureParameterSets = data[pos]; pos += 1
	record.PictureParameterSets = make([]ParameterSet, record.numOfPictureParameterSets)
	for i in 0..<record.numOfPictureParameterSets {
		pps := &record.PictureParameterSets[i]
		pps.length = ((^u16be)(&data[pos]))^
		pos += size_of(u16be)
		pps.data = make([]u8, pps.length)
		for &val in pps.data {
			val = data[pos]
			pos += 1
		}
	}

	// Optional block for advanced profiles (e.g. High profile = 100)
  if record.AVCProfileIndication == 100 || record.AVCProfileIndication == 110 ||
	   record.AVCProfileIndication == 122 || record.AVCProfileIndication == 144 {

		record.chroma_format = (data[pos] & 0b00000011); pos += 1
		record.bit_depth_luma_minus8 = (data[pos] & 0b00000111); pos += 1
		record.bit_depth_chroma_minus8 = (data[pos] & 0b00000111); pos += 1
		record.numOfSequenceParameterSetExt = data[pos]; pos += 1

		record.SequenceParameterSetExt = make([]ParameterSet, record.numOfSequenceParameterSetExt)
		for i in 0..<record.numOfSequenceParameterSetExt {
			sppe := &record.SequenceParameterSetExt[i]
			sppe.length = ((^u16be)(&data[pos]))^
			pos += size_of(u16be)
			sppe.data = make([]u8, sppe.length)
			for &val in sppe.data {
				val = data[pos]
				pos += 1
			}
		}
	}
}



// // btrt
// MPEG4BitRateBox :: struct #packed {
//  unsigned int(32) bufferSizeDB;
//  unsigned int(32) maxBitrate;
//  unsigned int(32) avgBitrate;
// }

// class MPEG4ExtensionDescriptorsBox extends Box(‘m4ds’) {
//  Descriptor Descr[0 .. 255];
// }
// class AVCSampleEntry() extends VisualSampleEntry (‘avc1’){
//  AVCConfigurationBox config;
//  MPEG4BitRateBox (); // optional
//  MPEG4ExtensionDescriptorsBox (); // optional
// }
// class AVC2SampleEntry() extends VisualSampleEntry (‘avc2’){
//  AVCConfigurationBox avcconfig;
//  MPEG4BitRateBox bitrate; // optional
//  MPEG4ExtensionDescriptorsBox descr; // optional
//  extra_boxes boxes; // optional
// } 