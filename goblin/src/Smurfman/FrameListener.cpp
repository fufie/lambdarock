#include "stdafx.h"
#include "FrameListener.hpp"
#include "App.hpp"

MyFrameListener::MyFrameListener(RenderWindow* win, Camera* cam, SmurfApp *app)
: ExampleFrameListener(win, cam), m_app(app)
{

}

// override any keys
bool 
MyFrameListener::processUnbufferedKeyInput(const FrameEvent& evt) {
	if(mKeyboard->isKeyDown(OIS::KC_I)) {
		// north
		if (m_app) {
			m_app->movePlayer(MOVE_NORTH);
		}
	}
	if(mKeyboard->isKeyDown(OIS::KC_J)) {
		// west
		if (m_app) {
			m_app->movePlayer(MOVE_WEST);
		}
	}
	if(mKeyboard->isKeyDown(OIS::KC_K)) {
		// south
		if (m_app) {
			m_app->movePlayer(MOVE_SOUTH);
		}
	}
	if(mKeyboard->isKeyDown(OIS::KC_L)) {
		// east
		if (m_app) {
			m_app->movePlayer(MOVE_EAST);
		}
	}
	// always jump further:
	return ExampleFrameListener::processUnbufferedKeyInput(evt);
}

