#include "stdafx.h"

#include "App.hpp"
#include "FrameListener.hpp"
#include "Player.hpp"

SmurfApp::SmurfApp() 
: m_dng("d:/dungeon.txt") {
	m_player = new Player(1,1);
}

SmurfApp::~SmurfApp() {
	// delete ninja?
}

void 
SmurfApp::drawAxisLines(double length) {
	ManualObject *mAxis = mSceneMgr->createManualObject("MyAxis");
	mAxis->begin( "BaseWhiteNoLighting", Ogre::RenderOperation::OT_LINE_LIST );
	mAxis->position(10,0,0);
	mAxis->colour(1,0,0);
	mAxis->position(length,0,0);
	mAxis->position(0,10,0);
	mAxis->colour(0,1,0);
	mAxis->position(0,length,0);
	mAxis->position(0,0,10);
	mAxis->colour(0,0,1);
	mAxis->position(0,0,length);
	mAxis->end();
	mSceneMgr->getRootSceneNode()->createChildSceneNode()->attachObject(mAxis);
}

void 
SmurfApp::createCamera(void) {
	// create camera, but leave at default position
	mCamera = mSceneMgr->createCamera("PlayerCam"); 
	mCamera->setNearClipDistance(5);

	mCamera->setPosition(0, 0, -10);
	mCamera->lookAt(0, 0, 10);
}

SceneNode *
SmurfApp::createNinja() {
	Entity *ent = mSceneMgr->createEntity("Ninja", "ninja.mesh");
	SceneNode *node = mSceneMgr->getRootSceneNode()->createChildSceneNode("NinjaNode");
	double scaleFactor = 0.1;
	node->scale(Ogre::Vector3(scaleFactor,scaleFactor, scaleFactor));
	node->attachObject(ent);
	return node;
}

void 
SmurfApp::placeCreature(Creature *creature) {
	// hack to put things on the ground
	if (!creature) return;

	Vector3 pos(squarePosition(creature->getX(),creature->getY()));
	pos.y -= SQUARE_LENGTH/2;
	creature->getNode()->setPosition(pos);
}

void 
SmurfApp::movePlayer(ActiveMove dir) {
	// move, if we can
	if (!m_player) return;

	unsigned x = m_player->getX();
	unsigned y = m_player->getY();

	switch (dir) {
		case MOVE_NORTH: y++; break;
		case MOVE_SOUTH: y--; break;
		case MOVE_EAST: x++; break;
		case MOVE_WEST: x--; break;
	}

	if (m_dng.get_coord(x,y) == 1) {
		// oops, cant move
	}
	else {
		m_player->setPosition(x,y);
		placeCreature(m_player);
	}

}


void 
SmurfApp::createScene(void) {
	// just keep some ambientlight now when testing
	mSceneMgr->setAmbientLight(ColourValue(1, 1, 1));

	drawAxisLines(300.0);

	ManualObject* manual = mSceneMgr->createManualObject("manual");
	drawDungeon(manual, m_dng);

	SceneNode *node = mSceneMgr->getRootSceneNode()->createChildSceneNode();
	node->attachObject(manual);

	// let's make a player
	m_player->setNode(createNinja());

	placeCreature(m_player);
	//placeObject(playerNode, 1,1);
}


// need to respond to keys
void 
SmurfApp::createFrameListener(void) {
	// Create the FrameListener
	mFrameListener = new MyFrameListener(mWindow, mCamera, this);
	// Show the frame stats overlay
	mFrameListener->showDebugOverlay(true);
	mRoot->addFrameListener(mFrameListener);
}

Vector3 
SmurfApp::squarePosition(unsigned x, unsigned y) {
	const unsigned height = m_dng.get_height();
	return Vector3(SQUARE_LENGTH*x,0.0,SQUARE_LENGTH*(height-y));
}

void 
SmurfApp::drawDungeon(ManualObject *manual, const Dungeon &dng) {
	const unsigned width = dng.get_width();
	const unsigned height = dng.get_height();
	std::ofstream foo("d:/masks.txt", std::ios::out);
	for (unsigned y = 0; y < height; ++y) {
		for (unsigned x = 0; x < width; ++x) {
			unsigned short mask = dng.get_mask_for_coord(x,y);
			foo << y << "," << x << ": ";
			if (mask & WALL_FLOOR) foo << "FLOOR ";
			if (mask & WALL_NORTH) foo << "NORTH ";
			if (mask & WALL_SOUTH) foo << "SOUTH ";
			if (mask & WALL_WEST) foo << "WEST ";
			if (mask & WALL_EAST) foo << "EAST ";
			if (mask & WALL_ROOF) foo << "ROOF ";
			// dungeon coords have opposite y to what we draw
			Vector3 squarepos(squarePosition(x,y));
			foo << " to " << squarepos << std::endl;

			addSquare(manual, mask, SQUARE_SIZE, squarepos, "Examples/BumpMapping/MultiLight"); //"Examples/Rockwall");
		}
	}
}

void 
SmurfApp::addSquare(ManualObject *obj, unsigned short mask, Vector3 dim, Vector3 pos, const char *texture) {
	bool reverseNormal = false;
	if (mask & WALL_NORTH) addWall(obj, dim, pos, texture, WALL_NORTH, reverseNormal);
	if (mask & WALL_SOUTH) addWall(obj, dim, pos, texture, WALL_SOUTH, reverseNormal);
	if (mask & WALL_WEST)  addWall(obj, dim, pos, texture, WALL_WEST, reverseNormal);
	if (mask & WALL_EAST)  addWall(obj, dim, pos, texture, WALL_EAST, reverseNormal);
	if (mask & WALL_FLOOR) addWall(obj, dim, pos, "Examples/BumpyMetal", WALL_FLOOR, reverseNormal);
	if (mask & WALL_ROOF)  addWall(obj, dim, pos, texture, WALL_ROOF, reverseNormal);
	reverseNormal = true;
	if (mask & WALL_REV_NORTH) addWall(obj, dim, pos, texture, WALL_NORTH, reverseNormal);
	if (mask & WALL_REV_SOUTH) addWall(obj, dim, pos, texture, WALL_SOUTH, reverseNormal);
	if (mask & WALL_REV_WEST)  addWall(obj, dim, pos, texture, WALL_WEST, reverseNormal);
	if (mask & WALL_REV_EAST)  addWall(obj, dim, pos, texture, WALL_EAST, reverseNormal);
	if (mask & WALL_REV_FLOOR) addWall(obj, dim, pos, "Examples/BumpyMetal", WALL_FLOOR, reverseNormal);
	if (mask & WALL_REV_ROOF)  addWall(obj, dim, pos, texture, WALL_ROOF, reverseNormal);
}

void 
SmurfApp::drawSingleWall(ManualObject *obj, 
						 Vector3 upperLeft, 
						 Vector3 lowerLeft, 
						 Vector3 lowerRight,	
						 Vector3 upperRight,
						 Vector3 basePos) 
{

	Ogre::Real tfx = 4.0;

	obj->position(upperLeft + basePos);
	obj->textureCoord(0,0);
	obj->position(lowerLeft + basePos);
	obj->textureCoord(0,tfx);
	obj->position(lowerRight + basePos);
	obj->textureCoord(tfx,tfx);
	obj->position(upperRight + basePos);
	obj->textureCoord(tfx,0);

}

void 
SmurfApp::addWall(ManualObject* obj, Vector3 dim, Vector3 pos, const char *texture, short mask, bool reverseNormal) {
	if (!obj) return;
	obj->begin(texture, Ogre::RenderOperation::OT_TRIANGLE_LIST);
	dim /= 2;
	Ogre::Real l = dim.x;
	Ogre::Real h = dim.y;
	Ogre::Real w = dim.z;

	if (mask & WALL_NORTH) {
		drawSingleWall(obj, 
			Ogre::Vector3(-l, h, w),
			Ogre::Vector3(-l, -h, w),
			Ogre::Vector3(l, -h, w),
			Ogre::Vector3(l, h, w), pos);
	}
	else if (mask & WALL_SOUTH) {
		drawSingleWall(obj, 
			Ogre::Vector3(l, h, -w),
			Ogre::Vector3(l, -h, -w), 
			Ogre::Vector3(-l, -h, -w),
			Ogre::Vector3(-l, h, -w), pos);
	}
	else if (mask & WALL_EAST) {
		drawSingleWall(obj, 
			Ogre::Vector3(l, h, w),
			Ogre::Vector3(l, -h, w), 
			Ogre::Vector3(l, -h, -w),
			Ogre::Vector3(l, h, -w), pos);
	}
	else if (mask & WALL_WEST) {
		drawSingleWall(obj, 
			Ogre::Vector3(-l, h, -w),
			Ogre::Vector3(-l, -h, -w), 
			Ogre::Vector3(-l, -h, w),
			Ogre::Vector3(-l, h, w), pos);
	}
	else if (mask & WALL_FLOOR) {
		drawSingleWall(obj, 
			Ogre::Vector3(-l, -h, w),
			Ogre::Vector3(-l, -h, -w), 
			Ogre::Vector3(l, -h, -w),
			Ogre::Vector3(l, -h, w), pos);
	}
	else if (mask & WALL_ROOF) {
		drawSingleWall(obj, 
			Ogre::Vector3(-l, h, -w),
			Ogre::Vector3(-l, h, w), 
			Ogre::Vector3(l, h, w),
			Ogre::Vector3(l, h, -w), pos);
	}

	if (reverseNormal) {
	obj->quad(0,1,2,3);
	}
	else {
	obj->quad(3,2,1,0);
	}
	obj->end();
}