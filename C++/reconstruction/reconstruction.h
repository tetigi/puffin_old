// ==========================================================
// Cinesite 2011 Inspire Intern test
// ==========================================================

#pragma once 

#include <vector>
#include <iostream>
#include <fstream>
#include <math.h>

#include "img1.h"
#include "img2.h"
#include "img3.h"

#include "../common.h"

// TestData
// ==========================================================
class TestData
{
public:
	TestData()
	{
	 	images = new Image*[3];

		static unsigned char *data[3] = {img1_data, img2_data, img3_data};

	 	for (int i = 0; i < 3; ++i)
	 	{
	 		int w,h;
	 		GetSize(i, w, h);

	 		images[i] = new Image(w, h);
	 		Image& img = *images[i];
	 		unsigned char *p = data[i];
	 		for (int y = 0; y < h; ++y)
	 		{
	 			for(int x = 0; x < w; ++x)
	 			{
	 				unsigned char* pixel = &p[(y * w + x) * 4];
	 				img(x,(h - y - 1)) = Color(pixel[0] / 255.0f, pixel[1] / 255.0f, pixel[2] / 255.0f, 1.0f);
	 			}
	 		}
	 	}
	}

	~TestData()
	{
		for (int i = 0; i < 3; ++i)
			delete images[i];

		delete[] images;
	}

	void GetSize(int index, int& width, int& height) const
	{
		static int dimensions[] = { img1_width, img1_height,
						 img2_width, img2_height,
						 img3_width, img3_height };

		width = dimensions[index * 2 + 0];
		height = dimensions[index * 2 + 1];
	}

	int GetNumSamples(int index) const
	{
		int w, h;
		GetSize(index, w, h);
		return (int) (0.25 * w * h);
	}

	Color Sample(int index, int x, int y) const
	{
		static int samples[] = { 0, 0, 0};

		samples[index]++;

		if (samples[index] <= GetNumSamples(index))
			return (*images[index])(x,y);
		else
			return Color(0,0,0,0);
	}

	Color CalculateError(int index, const Image& image) const
	{
		return images[index]->CalculateRmsError(image);
	}

private:
	Image** images;
};
