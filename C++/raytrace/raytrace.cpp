// ==========================================================
// Cinesite 2011 Inspire Intern test
// ==========================================================

#include <iostream>
#include <algorithm>
#include <sstream>
#include "raytrace.h"
using namespace std;


Intersection Intersect(const Scene& scene, const Ray& ray)
{
	Intersection intersection;

	for (size_t i = 0; i < scene.GetPlanes().size(); ++i)
	{
		const Plane& plane = scene.GetPlanes()[i];
		Intersection tmpIntersection;
		if (plane.Intersect(ray, intersection) && tmpIntersection < intersection)
			intersection = tmpIntersection;
	}

	for (size_t i = 0; i < scene.GetSpheres().size(); ++i)
	{
		const Sphere& sphere = scene.GetSpheres()[i];
		Intersection tmpIntersection;
		if (sphere.Intersect(ray, intersection) && tmpIntersection < intersection)
			intersection = tmpIntersection;
	}

	return intersection;
}

Color Trace(const Scene& scene, const Ray& ray, int depth)
{
	Intersection intersection = Intersect(scene, ray);

	Color c(0,0,0,0);

	if (intersection.Intersected())
	{
		c.a = 1.0f;
		for (unsigned int l = 0; l < scene.GetLights().size(); ++l)
		{
			const Light& light = scene.GetLights()[l];
			Vector lightDir = light.Position() - intersection.Position();
			float d = lightDir.Length();
			float attenuation = light.Itensity() / (d * d);
			lightDir = ::Normalize(lightDir);

			Ray shadowRay(intersection.Position() + intersection.Normal() * 0.0001f, lightDir);

			if (!Intersect(scene, shadowRay).Intersected())
			{
				float a = std::max(0.0f, intersection.Normal().Dot(lightDir)) * attenuation;
				c += Color(a,a,a, 0.0);
			}
		}
	}

	return c;
}

int main()
{
	// create an image to render to
	Image img(1024, 1024);
	
	// create a scene
	Scene scene;
	
	// render 10 frames
	for (int i = 0; i < 10; ++i)
	{
		scene.SetTime((float) i);
		const Camera& camera  = scene.GetCamera();

		float pixelWidth = 1.0f / (float) img.width;
		float pixelHeight = 1.0f / (float) img.height;

		for (int y = 0; y < img.height; ++y)
		{
			for (int x = 0; x < img.width; ++x)
			{
				img(x, y) = Color(0,0,0,0);

				float dx = (x * pixelWidth) - 0.5f;
				float dy = (y * pixelHeight) - 0.5f;

				Ray cameraSpaceRay  = Ray(Vector(0,0,0,1), Vector(dx, dy, 1.0f, 0.0f));
				Ray ray = camera.Transform() * cameraSpaceRay;

				int depth = 0;
				img(x, y) += Trace(scene, ray, depth);
			}
		}
		std::stringstream ss;
		ss << "img";
		ss.width(4);
		ss.fill('0');
		ss << (i+1) << ".tga";
		img.Write(ss.str().c_str());
	}

	return 0;
}
