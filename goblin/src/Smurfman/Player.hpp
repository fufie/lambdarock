#ifndef SMURF_PLAYER_HPP
#define SMURF_PLAYER_HPP

#include "ExampleApplication.h"

class Creature {
public:
	Creature(unsigned x, unsigned y);

	virtual SceneNode *getNode();
	virtual void setNode(SceneNode *node);
	virtual unsigned getX() const;
	virtual unsigned getY() const;
	virtual void setPosition(unsigned x, unsigned y);

private:
	SceneNode *creatureNode;
	unsigned pos_x;
	unsigned pos_y;
};

class Player : public Creature {
public:
	Player(unsigned x, unsigned y);

};

#endif