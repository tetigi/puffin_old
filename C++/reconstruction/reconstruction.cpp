// ==========================================================
// Cinesite 2011 Inspire Intern test
// ==========================================================

#include <iostream>
#include "reconstruction.h"
#include <sstream>
#include <iostream>

Image* Reconstruct(const TestData& d, int p)
{
	int w,h;
	d.GetSize(p, w, h);
	Image* out = new Image(w, h);

	std::cout << w << " " << h << std::endl;
	for (int i = 0; i < w; ++i)
	{
		for(int j = 0; j < h; ++j)
		{
			(*out)(i,j) = d.Sample(p, i, j);
		}
	}

	Color colorError = d.CalculateError(p, *out);
	std::cout << "error:" << colorError << std::endl;
	return out;
}

int main()
{
	TestData d;

	for (int p = 0; p < 3; ++p)
	{
		Image* image = Reconstruct(d, p);

		std::stringstream ss;
		ss << "reconstruct" << p << ".tga";

		image->Write(ss.str().c_str());

		delete image;
	}

	return 0;
}
