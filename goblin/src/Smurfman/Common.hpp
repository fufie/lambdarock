#ifndef SMURF_COMMON_HPP
#define SMURF_COMMON_HPP

#ifndef _WIN32_WINNT            // Specifies that the minimum required platform is Windows Vista.
#define _WIN32_WINNT 0x0600     // Change this to the appropriate value to target other versions of Windows.
#endif

enum WallDraw {
	WALL_NONE  = 0x0000,
	WALL_NORTH = 0x0001,
	WALL_EAST  = 0x0002,
	WALL_WEST  = 0x0004,
	WALL_SOUTH = 0x0008,
	WALL_FLOOR = 0x0010,
	WALL_ROOF  = 0x0020,
	// opposite normals
	WALL_REV_NORTH = 0x0040, 
	WALL_REV_EAST  = 0x0080, 
	WALL_REV_WEST  = 0x0100, 
	WALL_REV_SOUTH = 0x0200, 
	WALL_REV_FLOOR = 0x0400, 
	WALL_REV_ROOF  = 0x0800, 
	WALL_ALL       = 0xFFFF
};

enum ActiveMove {
	MOVE_NORTH,
	MOVE_WEST,
	MOVE_EAST,
	MOVE_SOUTH
};

#define SQUARE_LENGTH 30.0
#define SQUARE_SIZE Vector3(SQUARE_LENGTH, SQUARE_LENGTH, SQUARE_LENGTH)

#endif
