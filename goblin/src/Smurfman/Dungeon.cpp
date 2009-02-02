#include "stdafx.h"

#include "Dungeon.hpp"
#include <fstream>
#include <iostream>

std::string
slurp_file(const std::string &filename) {
	std::string narrow_fname(filename);

	FILE *infile = fopen(narrow_fname.c_str(), "rb");
	if (!infile) {
		std::cerr << "Could not read from external file named '" << narrow_fname << "'" << std::endl;
		return "";
	}

	fseek(infile, 0, SEEK_END);
	long size = ftell(infile);
	rewind(infile);
	char *ptr = new char[size+1];

	if (fread(ptr, size, 1, infile)) {
		std::string value(ptr, size);
		delete[] ptr;
		return value;
	}
	else {
		delete[] ptr;
		std::cerr << "Could not read data from external file named '" << narrow_fname << "'" << std::endl;
		return "";
	}
}

Dungeon::Dungeon(unsigned width, unsigned height) 
: m_width(width), m_height(height) {
	m_map = new char[m_width*m_height+1];
	for (unsigned y=0; y < m_height; ++y) {
		for (unsigned x=0; x < m_width; ++x) {
			if (x == 0 || y == 0 || x == m_width-1 || y == m_height-1)
				get_coord(x,y) = 1;
			else if (x == 2 && y == 2)
				get_coord(x,y) = 1;
			else
				get_coord(x,y) = 0;
		}
	}
}
Dungeon::Dungeon(const std::string &filename) {
	std::string data(slurp_file(filename));
	const unsigned sz = data.size();
	m_width = data.find('\n')-1;
	m_height = sz / (m_width+1);
	m_map = new char[m_width*m_height+1];

	unsigned cur_x=0;
	unsigned cur_y=0;
	for (unsigned i=0; i < sz; ++i) {
		char c = data[i];
		if (c == '#') {
			get_coord(cur_x, cur_y) = 1;
			cur_x++;
		}
		else if (c == '.') {
			get_coord(cur_x, cur_y) = 0;
			cur_x++;
		}
		else if (c == '\n') {
			cur_x = 0;
			cur_y++;
		}
	}

	std::ofstream foo("d:/dump.txt", std::ios::out);
	dump(foo);
}
Dungeon::~Dungeon() {
	delete[] m_map;
}

unsigned Dungeon::get_width() const {
	return m_width;
}

unsigned Dungeon::get_height() const {
	return m_height;
}

// optimise later
char &
Dungeon::get_coord(unsigned x, unsigned y) {
	return m_map[y*m_width+x];
}
const char &
Dungeon::get_coord(unsigned x, unsigned y) const {
	return m_map[y*m_width+x];
}

void 
Dungeon::dump(std::ostream &os) const {
	os << "Height: " << m_height << std::endl;
	os << "Width: " << m_width << std::endl;
	for (unsigned y=0; y < m_height; ++y) {
		for (unsigned x=0; x < m_width; ++x) {
			if (get_coord(x,y) == 1)
				os << "#";
			else 
				os << "."; 
		}
		os << std::endl;
	}
	os << std::endl;
}

unsigned short 
Dungeon::get_mask_for_coord(unsigned x, unsigned y) const {

	if (get_coord(x,y) == 1) {
		unsigned short mask = WALL_REV_ROOF | WALL_REV_FLOOR;
		if (x == 0) mask |= WALL_REV_WEST;
		if (y == 0) mask |= WALL_REV_NORTH;
		if (x == m_width-1) mask |= WALL_REV_EAST;
		if (y == m_height-1) mask |= WALL_REV_SOUTH;

		return mask;
	}
	else {
		unsigned short mask = WALL_FLOOR;
		if (get_coord(x,y-1) == 1) mask |= WALL_NORTH;
		if (get_coord(x,y+1) == 1) mask |= WALL_SOUTH;
		if (get_coord(x-1,y) == 1) mask |= WALL_WEST;
		if (get_coord(x+1,y) == 1) mask |= WALL_EAST;
		return mask;
	}
}
