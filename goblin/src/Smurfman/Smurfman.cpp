// Smurfman.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include "ExampleApplication.h"
#include "App.hpp"

#if OGRE_PLATFORM == OGRE_PLATFORM_WIN32
#define WIN32_LEAN_AND_MEAN
#include "windows.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if OGRE_PLATFORM == OGRE_PLATFORM_WIN32
	INT WINAPI WinMain( HINSTANCE hInst, HINSTANCE, LPSTR strCmdLine, INT )
#else
	int main(int argc, char **argv)
#endif
	{
		// Create application object
		SmurfApp app;

		try {
			app.go();
		} catch( Exception& e ) {
#if OGRE_PLATFORM == OGRE_PLATFORM_WIN32 
			MessageBox( NULL, e.what(), "An exception has occurred!", MB_OK | MB_ICONERROR | MB_TASKMODAL);
#else
			fprintf(stderr, "An exception has occurred: %s\n",
				e.what());
#endif
		}

		return 0;
	}

#ifdef __cplusplus
}
#endif
