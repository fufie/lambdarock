#include "stdafx.h"
#include "Player.hpp"

Creature::Creature(unsigned x, unsigned y) 
: pos_x(x), pos_y(y), creatureNode(NULL) {

}

SceneNode *
Creature::getNode() {
	return creatureNode;
}

void
Creature::setNode(SceneNode* node) {
	if (creatureNode) {
		// delete?
	}
	creatureNode = node;
}

unsigned 
Creature::getX() const {
	return pos_x;
}

unsigned 
Creature::getY() const {
	return pos_y;
}

void 
Creature::setPosition(unsigned x, unsigned y) {
	pos_x = x;
	pos_y = y;
}

Player::Player(unsigned x, unsigned y) 
: Creature(x,y) {

}
