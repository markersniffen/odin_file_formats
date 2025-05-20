package iso_bmff

get_payload :: proc(root:^BMFF_Box, fourcc:FourCC, $T:typeid) -> ^T {
	box := get_box(root, fourcc)
	assert(box != nil)
	payload, ok := &box.payload.(T)
	assert(ok)
	return payload
}

get_box :: proc(root:^BMFF_Box, fourcc:FourCC) -> ^BMFF_Box {
	for box := root.first_child; box != nil; box = box.next {
		if box.header.type == fourcc do return box
		child := get_box(box, fourcc)
		if child != nil do return child
	}
	return nil
}

get_boxes :: proc(dst:^[dynamic]^BMFF_Box, root:^BMFF_Box, fourcc:FourCC) {
	for box := root.first_child; box != nil; box = box.next {
		if box.header.type == fourcc do append(dst, box)
		get_boxes(dst, box, fourcc)
	}
}