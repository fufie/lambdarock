#ifndef SMURF_APP_HPP
#define SMURF_APP_HPP

#include "stdafx.h"

#include "ExampleApplication.h"
#include "Dungeon.hpp"

class Player;
class Creature;
class SmurfApp : public ExampleApplication {
public:
	SmurfApp();

	~SmurfApp();

	void drawAxisLines(double length = 300.0);

	void createCamera(void);

	SceneNode *createNinja();

	void placeCreature(Creature *creature);
	//void placeObject(SceneNode *node, unsigned x, unsigned y);
	void movePlayer(ActiveMove dir);

	void createScene(void);

	// need to respond to keys
	void createFrameListener(void);

	Vector3 squarePosition(unsigned x, unsigned y);

	void drawDungeon(ManualObject *manual, const Dungeon &dng);

	void addSquare(ManualObject *obj, unsigned short mask, Vector3 dim, Vector3 pos, const char *texture);

	void drawSingleWall(ManualObject *obj, 
		Vector3 upperLeft, 
		Vector3 lowerLeft, 
		Vector3 lowerRight,	
		Vector3 upperRight,
		Vector3 basePos);

	void addWall(ManualObject* obj, 
		Vector3 dim, 
		Vector3 pos, 
		const char *texture, 
		short mask,
		bool reverseNormal = false) ;

protected:
	Dungeon m_dng;
	Player *m_player;
};

#endif 