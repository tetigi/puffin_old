// ==========================================================
// Cinesite 2011 Inspire Intern test
// ==========================================================
#pragma once

#include <float.h>
#include <vector>
#include <stdio.h>
#include <cstring>

#include "../common.h"

// 4x4 Matrix class
// ================
class Matrix
{
  public:
    Matrix();

    Matrix(float v11, float v12, float v13, float v14,
        float v21, float v22, float v23, float v24,
        float v31, float v32, float v33, float v34,
        float v41, float v42, float v43, float v44);

    Matrix(const Vector& column1,
        const Vector& column2,
        const Vector& column3,
        const Vector& column4);

    Vector Column(int column) const;
    Vector Right() const { return Column(0); }
    Vector Up() const { return Column(1); }
    Vector Forward() const { return Column(2); }

    const Vector& Row(int row) const;
    Vector& operator[](int row);
    const Vector& operator[](int row) const;
    float operator()(int row, int column) const;

    void operator*=(const Matrix& other);
    void operator+=(const Matrix& other);
    void operator-=(const Matrix& other);

    void Transpose();
    float Determinant() const;
    void Invert();
    void InvertSimple();

    bool HasScale() const;

    Vector GetTranslation() const { return Vector(m_row[0][3], m_row[1][3], m_row[2][3], m_row[3][3]); }
    void SetTranslation(const Vector& p)
    {
      m_row[0].Set(3, p.x());
      m_row[1].Set(3, p.y());
      m_row[2].Set(3, p.z());
      m_row[3].Set(3,1.0f);
    }

  private:
    Vector m_row[4];
};

Matrix operator*(const Matrix& lhs, const Matrix& rhs);
Matrix operator+(const Matrix& lhs, const Matrix& rhs);
Matrix operator-(const Matrix& lhs, const Matrix& rhs);

Vector operator*(const Matrix& lhs, const Vector& rhs);

Matrix LookAt(const Vector& eye, const Vector& focus, const Vector& up);

Matrix RotateX(float theta);
Matrix RotateY(float theta);
Matrix RotateZ(float theta);

inline Matrix::Matrix()
{
  m_row[0] = Vector(1.0f, 0.0f, 0.0f, 0.0f);
  m_row[1] = Vector(0.0f, 1.0f, 0.0f, 0.0f);
  m_row[2] = Vector(0.0f, 0.0f, 1.0f, 0.0f);
  m_row[3] = Vector(0.0f, 0.0f, 0.0f, 1.0f);
}

inline Matrix::Matrix(float v11, float v12, float v13, float v14,
    float v21, float v22, float v23, float v24,
    float v31, float v32, float v33, float v34,
    float v41, float v42, float v43, float v44)
{
  m_row[0] = Vector(v11, v12, v13, v14);
  m_row[1] = Vector(v21, v22, v23, v24);
  m_row[2] = Vector(v31, v32, v33, v34);
  m_row[3] = Vector(v41, v42, v43, v44);
}

inline Matrix::Matrix(const Vector& column1,
    const Vector& column2,
    const Vector& column3,
    const Vector& column4)
{
  for(int i = 0; i < 4; ++i)
    m_row[i] = Vector(column1[i], column2[i], column3[i], column4[i]);
}

inline Vector Matrix::Column(int column) const
{
  return Vector(m_row[0][column], m_row[1][column], m_row[2][column], m_row[3][column]);
}

inline const Vector& Matrix::Row(int row) const
{
  return m_row[row];
}

inline const Vector& Matrix::operator[] (int row) const
{
  return m_row[row];
}

inline Vector& Matrix::operator[](int row)
{
  return m_row[row];
}

inline float Matrix::operator()(int row, int column) const
{
  return m_row[row][column];
}

inline void Matrix::operator*=(const Matrix& other)
{
  Matrix tmp;

  for (int i = 0; i < 4; ++i)
  {
    for(int j = 0; j < 4; ++j)
    {
      float acc = 0.0f;
      for(int k = 0; k < 4; ++k)
      {
        acc += m_row[i][k] * other.m_row[k][j];
      }
      tmp[i].Set(j, acc);
    }
  }
  *this = tmp;
}

inline void Matrix::operator+=(const Matrix& other)
{
  for (int i = 0; i < 4; ++i)
    for(int j = 0; j < 4; ++j)
      m_row[i].Set(j, m_row[i][j] + other.m_row[i][j]);
}

inline void Matrix::operator-=(const Matrix& other)
{
  for (int i = 0; i < 4; ++i)
    for(int j = 0; j < 4; ++j)
      m_row[i].Set(j, m_row[i][j] - other.m_row[i][j]);
}

inline void Matrix::Transpose()
{
  for (int i = 0; i < 4; ++i)
    for(int j = 0; j < i; ++j)
    {
      float tmp = m_row[i][j];
      m_row[i].Set(j,m_row[j][i]);
      m_row[j].Set(i, tmp);
    }
}

inline float Matrix::Determinant() const
{
  return 1;
}

inline void Matrix::Invert()
{
}

inline void Matrix::InvertSimple()
{
  // transpose 3x3 part of the matrix.
  for (int i = 0; i < 3; ++i)
    for(int j = 0; j < i; ++j)
    {
      float tmp = m_row[i][j];
      m_row[i].Set(j,m_row[j][i]);
      m_row[j].Set(i, tmp);
    }

  Vector translation = GetTranslation();
  SetTranslation(Vector(0,0,0,1));

  Vector invertedTranslation = *this * (-translation);
  SetTranslation(invertedTranslation);
}

inline Matrix operator*(const Matrix& lhs, const Matrix& rhs)
{
  Matrix tmp(lhs);
  tmp *= rhs;
  return tmp;
}

inline Matrix operator+(const Matrix& lhs, const Matrix& rhs)
{
  Matrix tmp(lhs);
  tmp += rhs;
  return tmp;
}

inline Matrix operator-(const Matrix& lhs, const Matrix& rhs)
{
  Matrix tmp(lhs);
  tmp -= rhs;
  return tmp;
}

inline Vector operator*(const Matrix& lhs, const Vector& rhs)
{
  return Vector(lhs.Row(0).Dot4(rhs),
      lhs.Row(1).Dot4(rhs),
      lhs.Row(2).Dot4(rhs),
      lhs.Row(3).Dot4(rhs));

}

inline bool operator==(const Matrix& lhs, const Matrix& rhs)
{
  return std::memcmp(&lhs, &rhs, sizeof(Matrix)) == 0;
}

inline bool operator!=(const Matrix& lhs, const Matrix& rhs)
{
  return !(lhs == rhs);
}

// Ray class
// ==========================================================

class Ray
{
  public:
    Ray(const Vector& origin, const Vector& direction) 
      : m_origin(origin), m_direction(Normalize(direction)) {}

    Ray(const Ray& ray) 
      : m_origin(ray.m_origin), m_direction(ray.m_direction) {}

    const Vector& Origin() const { return m_origin; }
    const Vector& Direction() const { return m_direction; }

    Vector GetPosition(float rayParameter) const { return m_origin + m_direction * rayParameter; }
  private:
    Vector m_origin;
    Vector m_direction;
};

class Intersection
{
  public:
    Intersection(const Vector& position, const Vector& normal, float rayParameter)
      : m_position(position),
      m_normal(normal),
      m_rayParameter(rayParameter){}

    Intersection(const Intersection& other) : m_position(other.m_position), m_normal(other.m_normal),
    m_rayParameter(other.m_rayParameter){}

    Intersection() : m_rayParameter(FLT_MAX) {}

    const Vector& Position() const { return m_position; }
    const Vector& Normal() const { return m_normal; }

    float RayParameter() const { return m_rayParameter; }

    bool Intersected() const { return m_rayParameter != FLT_MAX; }
  private:
    Vector m_position;
    Vector m_normal;

    float m_rayParameter;
};

inline bool operator<(const Intersection& lhs, const Intersection& rhs)
{
  return lhs.RayParameter() < rhs.RayParameter();
}

// Sphere class
// ==========================================================

class Sphere
{
  public:
    Sphere(const Vector& center, float radius) 
      : m_center(center), m_radius(radius), m_radiusSquared(radius * radius)
    {}

    Sphere() 
    {}

    const Vector& Center() const { return m_center; }
    float Radius() const { return m_radius; }
    float RadiusSquared() const { return m_radiusSquared; }

    bool Intersect(const Ray& ray, Intersection& intersection) const
    {
      // calculate ray parameter
      float rayParameter;
      Vector p = Center() - ray.Origin();
      float pDotRayDir = p.Dot(ray.Direction());
      float radiusSquared = RadiusSquared();

      float temp = radiusSquared + pDotRayDir * pDotRayDir - p.Dot(p);
      if (temp < 0.0f)
        return false;

      rayParameter = pDotRayDir - sqrtf(temp);
      if (rayParameter  < 0.0f)
        return false;

      // normal & position
      Vector position = ray.GetPosition(rayParameter);
      Vector normal = Normalize(position - Center());

      intersection =  Intersection(position, normal, rayParameter);
      return true;
    }
  private:
    Vector m_center;
    float  m_radius;
    float  m_radiusSquared;
};

// Plane clas
// ==========================================================
class Plane
{
  public:
    Plane(const Vector& normal, float distance) : m_normal(normal), m_distance(distance) {}
    Plane() {}

    const Vector& Normal() const { return m_normal; }
    float Distance() const { return m_distance; }

    bool Intersect(const Ray& ray, Intersection& intersection) const
    {
      float rayParameter = (m_distance - m_normal.Dot(ray.Origin())) / m_normal.Dot(ray.Direction());

      if (rayParameter < 0.0f)
        return false;

      Vector position = ray.Origin() + ray.Direction() * rayParameter;

      intersection =  Intersection(position, m_normal, rayParameter);
      return true;
    }
  private:
    Vector m_normal;
    float m_distance;
};


inline Ray operator*(const Matrix& transform, const Ray& ray)
{
  return Ray(transform * ray.Origin(), transform * ray.Direction());
}

// Camera Class
// ==========================================================
class Camera
{
  public:
    Camera(const Matrix& transform, float near, float far)
      : m_transform(transform), m_near(near),
      m_far(far)
  {
    m_worldToCamera = transform;
    m_worldToCamera.InvertSimple();
  }

    const Matrix& Transform() const { return m_transform; }

    Vector ToCameraSpace(const Vector& worldPosition) const { return m_worldToCamera * worldPosition; }

    float NormalizedZ(float z) const
    {
      float nz = 1.0f - (z - m_near) / (m_far - m_near);
      if (nz < 0.0f)
        return 0.0f;
      if (nz > 1.0f)
        return 1.0f;

      return nz;
    }

  private:
    Matrix m_transform;
    Matrix m_worldToCamera;
    float m_near;
    float m_far;
};


class Light
{
  public:
    Light(const Vector& position, float intensity)
      : m_position(position),
      m_intensity(intensity)
  {}

    Light() {}

    const Vector& Position() const { return m_position; }
    float Itensity() const { return m_intensity; }

  private:
    Vector m_position;
    float m_intensity;
};

// Scene class
// ==========================================================
class Scene
{
  public:
    Scene()
      : m_camera(Matrix(), 1.0f, 50.0f)
    {
      SetTime(0.0f);
    }

    void SetTime(float time)
    {
      float t = Clamp(0.0f, 10.0f, time);
      float a = t / 10.0f;

      float d = Lerp(10.0f, 2.0f, a);

      m_spheres = std::vector<Sphere>(0);
      m_spheres.push_back(Sphere(Vector(0, 1.0f, 20.0f, 1.0f), 1.0f));
      m_spheres.push_back(Sphere(Vector(-d, 1.0f, 20.0f, 1.0f), 1.0f));
      m_spheres.push_back(Sphere(Vector(d, 1.0f, 20.0f, 1.0f), 1.0f));

      Matrix m;

      m_planes = std::vector<Plane>(0);
      m_planes.push_back(Plane(Vector(0.0f, 1.0f, 0.0f, 0.0f), 0.0f));

      float z = Lerp(10.0f, 20.0f, a);
      m_lights = std::vector<Light>(0);
      m_lights.push_back(Light(Vector(0.0f, 4.0f, z, 1.0f), 15.0f));

      float h = Lerp(1.0f, 5.0f, a);
      m.SetTranslation(Vector(0.0f, h, 0.0f, 1.0f));
      m_camera = Camera (m, 1.0f, 50.0f);
    }

    const std::vector<Sphere>& GetSpheres() const { return m_spheres; }
    const std::vector<Plane>& GetPlanes() const { return m_planes; }
    const std::vector<Light>& GetLights() const { return m_lights; }

    const Camera& GetCamera() const { return m_camera; }

  private:
    std::vector<Sphere> m_spheres;
    std::vector<Plane> m_planes;
    std::vector<Light> m_lights;
    Camera m_camera;
};
