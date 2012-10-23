// ==========================================================
// Cinesite 2011 Inspire Intern test
// ==========================================================

#pragma once

#include <iostream>
#include <math.h>
#include <vector>
#include <fstream>
#include <iomanip>
#include <algorithm>

template<typename T> T Lerp(const T& a, const T& b, float v)
{
	return a * (1.0 - v) + b * v;
}

template<typename T> T Clamp(const T& a, const T& b, const T& c)
{
	if (c < a)
		return a;
	if (c > b)
		return b;

	return c;
}

// 4 element vector class
// ==========================================================

class Vector
{
public:
	Vector(float a);
	Vector(float x, float y, float z, float w);
	Vector();
	Vector(const Vector& other);

	float x() const;
	float y() const;
	float z() const;
	float w() const;

	void Set(int index, float v);

	float Component(int index) const;
	float operator[] (int index) const;

	void operator+=(const Vector& other);
	void operator-=(const Vector& other);
	void operator*=(const Vector& other);
	void operator*=(float other);

	void operator/=(const Vector& other);
	void operator/=(float other);

	Vector operator-() const;

	float Dot(const Vector& other) const;
	float Dot4(const Vector& other) const;
	Vector Cross(const Vector& other) const;

	float LengthSquared() const;
	float Length() const;

	int LeastBasis() const;
	void Basis(Vector& e0, Vector& e1) const;
	void Normalize();
private:
	float m[4];
};

Vector Normalize(const Vector& v);


inline float Vector::x() const { return m[0]; }
inline float Vector::y() const { return m[1]; }
inline float Vector::z() const { return m[2]; }
inline float Vector::w() const { return m[3]; }

inline float Vector::operator[](int index) const { return m[index]; }

inline void Vector::Set(int index, float v)
{
	m[index] = v;
}

inline Vector::Vector(const Vector& other)
{
    m[0] = other.m[0];
    m[1] = other.m[1];
    m[2] = other.m[2];
    m[3] = other.m[3];
}

inline Vector::Vector(float x, float y, float z, float w)
{
        m[0] = x;
        m[1] = y;
        m[2] = z;
        m[3] = w;
}

inline Vector::Vector()
{
        m[0] = 0.0f;
        m[1] = 0.0f;
        m[2] = 0.0f;
        m[3] = 0.0f;
}

inline void Vector::operator+=(const Vector& other)
{
        m[0] += other.x();
        m[1] += other.y();
        m[2] += other.z();
        m[3] += other.w();
}

inline void Vector::operator-=(const Vector& other)
{
        m[0] -= other.x();
        m[1] -= other.y();
        m[2] -= other.z();
        m[3] -= other.w();
}

inline void Vector::operator*=(const Vector& other)
{
        m[0] *= other.x();
        m[1] *= other.y();
        m[2] *= other.z();
        m[3] *= other.w();
}

inline void Vector::operator*=(float other)
{
        m[0] *= other;
        m[1] *= other;
        m[2] *= other;
        m[3] *= other;
}

inline void Vector::operator/=(const Vector& other)
{
        m[0] /= other.x();
        m[1] /= other.y();
        m[2] /= other.z();
        m[3] /= other.w();
}

inline void Vector::operator/=(float other)
{
        m[0] /= other;
        m[1] /= other;
        m[2] /= other;
        m[3] /= other;
}

inline float Vector::Dot(const Vector& other) const
{
        return x() * other.x() + y() * other.y() + z() * other.z();
}

inline float Vector::Dot4(const Vector& other) const
{
        return x() * other.x() + y() * other.y() + z() * other.z() + w() * other.w();
}

inline Vector Vector::Cross(const Vector& other) const
{
        const float x1 = y() * other.z() - z() * other.y();
        const float y1 = z() * other.x() - x() * other.z();
        const float z1 = x() * other.y() - y() * other.x();
        return Vector(x1, y1, z1, 0.0f);
}

inline float Vector::LengthSquared() const
{
        return this->Dot(*this);
}

inline float Vector::Length() const
{
        return sqrtf(LengthSquared());
}

inline Vector operator+(const Vector& lhs, const Vector& rhs)
{
        Vector tmp(lhs);
        tmp += rhs;
        return tmp;
}

inline Vector operator-(const Vector& lhs, const Vector& rhs)
{
        Vector tmp(lhs);
        tmp -= rhs;
        return tmp;
}

inline Vector operator*(const Vector& lhs, const Vector& rhs)
{
        Vector tmp(lhs);
        tmp *= rhs;
        return tmp;
}

inline Vector operator*(const Vector& lhs, float rhs)
{
        Vector tmp(lhs);
        tmp *= rhs;
        return tmp;
}

inline Vector operator*(float lhs, const Vector& rhs)
{
        Vector tmp(rhs);
        tmp *= lhs;
        return tmp;
}

inline Vector operator/(const Vector& lhs, float rhs)
{
        Vector tmp(lhs);
        tmp /= rhs;
        return tmp;
}

inline Vector operator/(float lhs, const Vector& rhs)
{
        Vector tmp(rhs);
        tmp /= lhs;
        return tmp;
}

inline Vector Normalize(const Vector& v)
{
    return v / v.Length();
}

inline int Vector::LeastBasis() const
{
    float m = fabsf(x());
    int a = 0;
    if (fabsf(y()) < m)
    {
        m = fabsf(y());
        a = 1;
    }

    if (fabsf(z()) < m)
    {
        a = 2;
    }
    return a;
}

inline void Vector::Basis(Vector& e0, Vector &e1) const
{
    const Vector e2 = ::Normalize(*this);

    int a = e2.LeastBasis();

    Vector w0 = Vector(a == 0 ? 1.0f : 0.0f, a == 1 ? 1.0f : 0.0f, a == 2 ? 1.0f : 0, 0.0f );

    e0 = Cross(w0);
    e0 = ::Normalize(e0);

    e1 = e2.Cross(e0);
    e1 = ::Normalize(e1);
}

inline Vector Vector::operator-() const
{
    return Vector(-x(), -y(), -z(), -w());
}

std::ostream& operator<<(std::ostream& os, const Vector& v)
{
	os << v.x() << ", " << v.y() << ", " << v.z() << ", " << v.w();
	return os;
}


// floating point colour class
// ==========================================================
class Color
{
public:
	explicit Color(double r = 0.0, double g = 0.0, double b = 0.0, double a = 1.0) : r(r), g(g), b(b), a(a) {}

	Color& operator-=(const Color& other) { r -= other.r; g -= other.g; b -= other.b; a -= other.a; return *this; }
	Color& operator+=(const Color& other) { r += other.r; g += other.g; b += other.b; a += other.a; return *this; }
	Color& operator*=(const Color& other) { r *= other.r; g *= other.g; b *= other.b; a *= other.a; return *this; }
	Color& operator/=(double v) { r /= v; g /= v; b /= v; a /= v; return *this; }

	double r;
	double g;
	double b;
	double a;
};

inline Color operator-(const Color& lhs, const Color& rhs)
{
	Color tmp(lhs);
	tmp -= rhs;
	return tmp;
}

inline Color sqrt(const Color& color)
{
	return Color(sqrt(color.r), sqrt(color.g), sqrt(color.b), sqrt(color.a));
}

inline std::ostream& operator<<(std::ostream& os, const Color& c)
{
	os << "(" << c.r << ", " << c.g << ", " << c.b << ", " << c.a << ")";
	return os;
}

// Simple Image class
// Able to write out tga files
// ==========================================================
class Image
{
public:
	Image(int width, int height) : width(width), height(height), m_pixels(width * height) {}
	void Write( const char* filename);
	const Color& operator() (int x, int y) const { return m_pixels[x + y * width]; }
	Color& operator() (int x, int y) { return m_pixels[x + y * width]; }

	bool ValidPoint(int x, int y) const { return x >= 0 && x < width && y >= 0 && y < height; }
	Color CalculateRmsError(const Image& other);
	void WritePythonSource(const char* symbol, const char* filename);

	int width;
	int height;

private:
	std::vector<Color> m_pixels;
};

void Image::Write(const char* filename)
{
	unsigned char* pixelData = new unsigned char[width * height * 4];

	int offsetSource = 0;
	int width4 = width * 4;
	int width8 = width * 8;
	int offsetDest = (height - 1) * width4;
	for (int y = 0; y < height; y++) {
		for (int x = 0; x < width; x++) {
			const Color& c = (*this)(x, y);

			pixelData[offsetDest + 0] = (unsigned char)(Clamp(0, 255, (int) (c.b * 255))); // b
			pixelData[offsetDest + 1] = (unsigned char)(Clamp(0, 255, (int) (c.g * 255))); // g
			pixelData[offsetDest + 2] = (unsigned char)(Clamp(0, 255, (int) (c.r * 255))); // r
			pixelData[offsetDest + 3] = (unsigned char)(Clamp(0,  255, (int) (c.a * 255))); // a

			offsetSource++;
			offsetDest += 4;
		}
		offsetDest -= width8;
	}

	unsigned char header[] = {
		0, // ID length
		0, // no color map
		2, // uncompressed, true color
		0, 0, 0, 0,
		0,
		0, 0, 0, 0, // x and y origin
		(unsigned char)(width & 0x00FF),
		(unsigned char)((width & 0xFF00) >> 8),
		(unsigned char)(height & 0x00FF),
		(unsigned char)((height & 0xFF00) >> 8),
		32, // 32 bit bitmap
		32 };

	std::fstream f(filename, std::fstream::out | std::fstream::binary);

	f.write((char*) header, (int) sizeof(header));
	f.write((char*) pixelData, (int) width * height * 4);
	f.close();

	delete[] pixelData;
}

Color Image::CalculateRmsError(const Image& other)
{
	if (width != other.width || height != other.height)
	{
		return Color(-1,-1,-1,-1);
	}

	Color rms(0.0,0.0,0.0,0.0);
	for (int y = 0; y < height; ++y)
	{
		for (int x = 0; x < width; ++x)
		{
			Color d = (*this)(x,y) - other(x,y);
			d *= d;
			d /= (width * height);
			rms += d;
		}
	}

	return sqrt(rms);
}

void Image::WritePythonSource(const char* symbol, const char* filename)
{
	std::fstream f(filename, std::fstream::out);

	f << symbol << " = (" << width << ", " << height;
	f << ", ";
	f << "";

	f << std::hex;

	f.fill('0');

	for(int y = 0; y < height; ++y)
	{
		f << "'";
		for(int x = 0; x < width; ++x)
		{
			const Color& c = (*this)(x,y);

			f.width(2);
			f << (unsigned int) (std::min( c.r, 1.0) * 255);

			f.width(2);
			f << (unsigned int) (std::min( c.g, 1.0) * 255);

			f.width(2);
			f << (unsigned int) (std::min( c.b, 1.0) * 255);

			f.width(2);
			f << (unsigned int) std::min( c.a, 1.0) * 255;
		}
		f << "'" << std::endl;
	}
	f << std::dec;

	f << ")";
	f.close();
}
