// ==========================================================
// Cinesite 2011 Inspire Intern test
// ==========================================================

#include <vector>
#include <fstream>

#include <cstdlib>

#include "../common.h"

// Generate an approximately normally distributed number using the Central limit theorem
// =====================================================================================
float Random()
{
	float a = 0.0;
	for(int i = 0; i < 12 ;++i)
		a  += (std::rand() / (float)RAND_MAX);

	return (a - 6.0);
}

// Generate a random sample in the cuboid defined by 
// [-size.x / 2 , -size.y / 2, -size.z] - [size.x / 2 , size.y / 2, size.z]
// normally distributed around [0,0,0]

Vector sample(const Vector& size)
{
	Vector halfSize = size;

	return Vector(Random() * size.x(), Random() * size.y(), Random() * size.z(), 0.0f);
}

/// Class which defines the initial state of the particles
/// ======================================================
class InitialConditions
{
public:
	// Initial positions
	std::vector<Vector> positions;
	
	// Initial velocities
	std::vector<Vector> velocities;
	
	// masses
	std::vector<float> masses;

	int m_numObjects;

	void InitPositions()
	{
		Vector centres[2] = { Vector(4e2f, 5e2f, 0.0f, 1.0), Vector(7e2f, 8e2f, 0.0f, 1.0f)};
        Vector radius[2] = { Vector (1e2f, 1e2f, 0.5e2f, 0.0f), Vector(1e2f, 0.5e2f, 1e2f, 0.0f)};

        for (int g = 0; g < 2; ++g)
        {
            const Vector& centre = centres[g];

            for (int s = 0; s < m_numObjects; ++s)
            {
            	Vector p = centre + sample(radius[g]);
            	positions.push_back(p);
            }
		}
	}

	void InitVelocities()
	{
        for (int g = 0; g < 2; ++g)
        {
            for (int s = 0; s < m_numObjects; ++s)
            {
                velocities.push_back(g == 0 ?
                		Vector(0.0f, 0.0f, 0.0f, 0.0f) :
                		Vector(0.0f, 0.0f, 0.0f, 0.0f));
            }
		}
	}

	void InitMasses()
	{
		for (int g = 0; g < 2; ++g)
        {
            for (int s = 0; s < m_numObjects; ++s)
            	masses.push_back(1000.0f * Random() * 50.0f);
		}
	}
	
	InitialConditions(int numObjects)
	: m_numObjects(numObjects)
	{
		InitPositions();
		InitVelocities();
		InitMasses();
	}

};
