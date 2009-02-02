#include "stdafx.h"


	/*
	
	enum BoxParts
	{
		BOX_NONE	= 0x00,
		BOX_TOP		= 0x01,
		BOX_BOT		= 0x02,
		BOX_FRONT	= 0x04,
		BOX_BACK	= 0x08,
		BOX_LEFT	= 0x10,
		BOX_RIGHT	= 0x20,
		BOX_ALL		= 0xFF
	};



	void drawShit(ManualObject *obj) {

		Vector3 pos(40.0,40.0,40.0);
		Vector3 dim(40.0, 40.0, 40.0);

		//dim/=2;

		Ogre::Real l = dim.x;
		Ogre::Real h = dim.y;
		Ogre::Real w = dim.z;

		obj->begin("Examples/Rockwall", Ogre::RenderOperation::OT_TRIANGLE_LIST); 
		obj->position(Ogre::Vector3(-l, h, w) + pos);
		obj->textureCoord(0,0);
		obj->position(Ogre::Vector3(-l, -h, w) + pos);
		obj->textureCoord(0,2);
		obj->position(Ogre::Vector3(l, -h, w) + pos);
		obj->textureCoord(2,2);
		obj->position(Ogre::Vector3(l, h, w) + pos);
		obj->textureCoord(2,0);
		obj->quad(0,1,2,3);
		//obj->triangle(0,1,2);
		obj->end();

	}

	void addBox(ManualObject* obj, Vector3 dim, Vector3 pos, ColourValue color, short boxMask)
	{
		if(!obj)
			return;

		obj->begin("Examples/Rockwall", Ogre::RenderOperation::OT_TRIANGLE_LIST); 

		dim/=2;

		Ogre::Real l = dim.x;
		Ogre::Real h = dim.y;
		Ogre::Real w = dim.z;

		obj->position(Ogre::Vector3(-l, h, w) + pos);
		obj->textureCoord(0,0);
		//obj->colour(color);
		obj->position(Ogre::Vector3(-l, -h, w) + pos);
		obj->textureCoord(1,0);
		//obj->colour(color);
		obj->position(Ogre::Vector3(l, -h, w) + pos);
		obj->textureCoord(0,1);
		//obj->colour(color);
		obj->position(Ogre::Vector3(l, h, w) + pos);
		obj->textureCoord(1,1);
		//obj->colour(color);

		obj->position(Ogre::Vector3(-l, h, -w) + pos);
		//obj->colour(color);
		obj->position(Ogre::Vector3(-l, -h, -w) + pos); 
		//obj->colour(color);
		obj->position(Ogre::Vector3(l, -h, -w) + pos);
		//obj->colour(color);
		obj->position(Ogre::Vector3(l, h, -w) + pos);
		//obj->colour(color);

		// normalise to the inside
		if(boxMask & BOX_FRONT)
			obj->quad(3,2,1,0);
		if(boxMask & BOX_BACK)
			obj->quad(4,5,6,7);

		// top bottom
		if(boxMask & BOX_TOP)
			obj->quad(4,7,3,0);
		if(boxMask & BOX_BOT)
			obj->quad(6,5,1,2);

		// end caps
		if(boxMask & BOX_RIGHT)
			obj->quad(5,4,0,1);
		if(boxMask & BOX_LEFT)
			obj->quad(7,6,2,3);

		obj->end(); 
	}
	
	ColourValue getColourValue(unsigned _where) {
		if (_where % 2) return ColourValue(1.0,0.0,0.0);
		else return ColourValue(0.0,1.0,0.0);
	}

	void addCorridor(ManualObject* obj, unsigned steps, Vector3 startPos) {
		//unsigned mask = BOX_FRONT|BOX_BACK|BOX_BOT;
		unsigned mask = BOX_ALL ^ (BOX_RIGHT| BOX_LEFT);
		Vector3 curPos(startPos);
		Vector3 dim(40.0, 40.0, 40.0);
		std::string lightname("Light_");
		for (unsigned i=0; i < steps; ++i) {
			addBox(obj, dim, curPos, getColourValue(i), mask);
			lightname.push_back('A' + i);
			Light *light = mSceneMgr->createLight(lightname);
			light->setType(Light::LT_POINT);
			light->setPosition(curPos + Vector3(5, 5, 5));

			curPos += Vector3(40.0, 0.0, 0.0);
		}
	}
*/