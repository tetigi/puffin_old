// ==========================================================
// Cinesite 2011 Inspire Intern test
// ==========================================================

#include <iostream>
#include <sstream>
#include "gravity.h"

using namespace std;

int main() {

	// Get the initial positions
	InitialConditions init(1024 * 128);

	// construct an iamge
	Image img(1024, 1024);

	// Render the initial positions
	for(size_t i = 0; i < init.positions.size(); ++i)
	{
		const Vector& p = init.positions[i];
		int x = (int) p.x();
		int y = (int) p.y();

		if (!img.ValidPoint(x,y))
			continue;

		Color c = img(x,y);
		img(x, y) =  Color(0.2 + c.r, 0.2 + c.g, 0.2 + c.b, 1.0);

	}

	// Write out an image
	img.Write("gravity-init.tga");

	return 0;
}
