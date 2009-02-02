#ifndef SMURF_DUNGEON_HPP
#define SMURF_DUNGEON_HPP

class Dungeon {
public:
	Dungeon(unsigned width=5, unsigned height=5);
	Dungeon(const std::string &filename);
	~Dungeon();

	unsigned get_width() const;

	unsigned get_height() const;

	// optimise later
	char &get_coord(unsigned x, unsigned y);
	const char &get_coord(unsigned x, unsigned y) const;

	void dump(std::ostream &os) const;

	unsigned short get_mask_for_coord(unsigned x, unsigned y) const;

private:
	unsigned m_width;
	unsigned m_height;
	char *m_map;
};

#endif /* SMURF_DUNGEON_HPP */