#ifndef SMURF_FRAMELISTENER_HPP
#define SMURF_FRAMELISTENER_HPP

#include "ExampleApplication.h"

class SmurfApp;

class MyFrameListener : public ExampleFrameListener {
public:
	MyFrameListener(RenderWindow* win, Camera* cam, SmurfApp *app);
	// override any keys
	virtual bool processUnbufferedKeyInput(const FrameEvent& evt);

private:
	SmurfApp *m_app;
};

#endif
