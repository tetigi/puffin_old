# Cinesite 2011 Inspire Intern test
# =================================

import sys
sys.path.append("..")
from lib import *

def clamp(a, b, v):
    if v < a:
        return a
    if v > b:
        return b
    
    return v

def lerp(a, b, v):
    return a * (1.0 - v) + b *v

# 4x4 Matrix class
# ==================================================================

class Matrix(object):
    def __init__(self, 
                 col1 = Vector(1.0, 0.0, 0.0, 0.0), 
                 col2 = Vector(0.0, 1.0, 0.0, 0.0),
                 col3 = Vector(0.0, 0.0, 1.0, 0.0),
                 col4 = Vector(0.0, 0.0, 0.0, 1.0)):
        
        self.__r = [None, None, None, None]
        for i in xrange(4):
            self.__r[i] = Vector(col1[i], col2[i], col3[i], col4[i])
    
    def column(self, column):
        return Vector(self.__r[0][column], self.__r[1][column], self.__r[2][column], self.__r[3][column])
    
    def right(self):
        return self.column(0)
    
    def up(self):
        return self.column(1)
    
    def forward(self):
        return self.column(2)

    def row(self, row):
        return Vector(self.__r[row].x,self.__r[row].y, self.__r[row].z, self.__r[row].w)

    def value(self, column, row):
        return self.__r[row].value(column)
    
    def set_value(self, column, row, value):
        self.__r[row].set_value(column, value) 
               
    def __mul__(self, rhs):
        if type(rhs) is Vector:
            return Vector(self.__r[0].dot(rhs),
                          self.__r[1].dot(rhs),
                          self.__r[2].dot(rhs),
                          self.__r[3].dot(rhs))
        
        if type(rhs) is Matrix:
            m = Matrix
            for  i in xrange(4):
                for j in xrange(4):
                    acc = 0.0
                    for k in xrange(4):
                        acc += self.value(i,k) * rhs.value(k,j)
                    
                    m.set_value(j, i, acc)
                
            return m
        
        if type(rhs) is Ray:
            return Ray(self * rhs.origin, self * rhs.direction)
        
    def transpose(self):
        for i in xrange(4):
            for j in xrange(4):
                tmp = self.value(i, j)
                self.set_value(i, j, self.value(j, i))
                self.set_value(j, i, tmp)
        
    def invert_simple(self):
        for i in xrange(3):
            for j in xrange(3):
                tmp = self.value(i, j)
                self.set_value(i, j, self.value(j, i))
                self.set_value(j, i, tmp)
                
        translation = self.get_translation()
        self.set_translation(Vector(0,0,0,1))
        
        inverted_translation = self * translation.neg();
        self.set_translation(inverted_translation)
        
    def get_translation(self):
        return Vector(self.value(3,0), self.value(3,1), self.value(3,2), self.value(3,3))
    
    def set_translation(self, translation):
        self.set_value(3, 0, translation.x)
        self.set_value(3, 1, translation.y)
        self.set_value(3, 2, translation.z)
        self.set_value(3, 3, 1.0)
        
    def invert(self):
        pass

# Ray class
# ==================================================================

class Ray(object):
    def __init__(self, origin, direction):
        self.origin = origin
        self.direction = normalize(direction)

    def get_position(self, ray_parameter):
        return self.origin + self.direction * ray_parameter

# Intersection
# ==================================================================
    
class Intersection(object):
    def __init__(self, position = None, normal = None, ray_parameter= sys.float_info.max, intersected = False):
        self.position = position
        self.normal = normal
        self.ray_parameter = ray_parameter
        self.intersected = intersected
        

# Sphere
# ==================================================================

class Sphere(object):
    def __init__(self, position, radius):
        self.position = position
        self.radius = radius
        self.radius_squared = radius * radius 

    
    def intersect(self, ray):
        # calculate ray parameter
        p = self.position - ray.origin
        pDotRayDir = p.dot(ray.direction)
        radiusSquared = self.radius_squared

        temp = radiusSquared + pDotRayDir * pDotRayDir - p.dot(p);
        if (temp < 0.0):
            return Intersection()

        rayParameter = pDotRayDir - math.sqrt(temp)
        if (rayParameter  < 0.0):
            return Intersection()

        # normal & position
        position = ray.get_position(rayParameter)
        normal = normalize(position - self.position)

        return  Intersection(position, normal, rayParameter, True);

# Cuboid
# ==================================================================

class Cuboid(object):
  def __init__(self, position, length, width, height):
    self.position = position
    self.length = length
    self.width = width
    self.height = height

  def intersect(self, ray):
    pass
    #TODO cuboid intersection

# Plane (infinite)
# ==================================================================

class Plane(object):
    def __init__(self, normal, distance):
        self.normal = normal
        self.distance = distance 
    
    def intersect(self, ray):
        
        d = self.normal.dot(ray.direction)
        if d < 1e-12 and d > -1e-12:
            return Intersection()
        
        rayParameter = (self.distance - self.normal.dot(ray.origin)) / d;

        if (rayParameter < 0.0):
            return Intersection()

        position = ray.get_position(rayParameter)

        return  Intersection(position, self.normal, rayParameter, True);
        
# Camera
# ==================================================================
        
class Camera(object):
    def __init__(self, transform, near, far):
        self.transform = transform
        self.world_to_camera = copy.deepcopy(self.transform)
        self.world_to_camera.invert_simple()
        
        self.near = near
        self.far = far
        
# Material TODO
# ==================================================================

class Material(object):
  def __init(self):
    #TODO Some material properties
    pass

# Light
# ==================================================================
        
class Light(object):
    def __init__(self, position, intensity):
        self.position = position
        self.intensity = intensity
            
# Scene
# ==================================================================

class Scene(object):
    def __init__(self):
        self.time = 0.0
        self.spheres = []
        self.planes = []
        self.lights = []

    def set_time(self, time):
        t = clamp(0.0, 10.0, time)
        a = t / 10.0
        
        d = lerp(10.0, 2.0, a)
        self.spheres = []
        self.spheres.append(Sphere(Vector(0.0, 1.0, 20.0, 1.0), 1.0))
        self.spheres.append(Sphere(Vector(-d, 1.0, 20.0, 1.0), 1.0))
        self.spheres.append(Sphere(Vector(d, 1.0, 20.0, 1.0), 1.0))
        
        self.planes = []
        self.planes.append(Plane(Vector(0.0, 1.0, 0.0, 0.0), 0.0))
        
        m = Matrix()
        z = lerp(10.0, 20.0, a)
        self.lights = []
        self.lights.append(Light(Vector(0.0, 4.0, z, 1.0), 15.0))
        
        h = lerp(1.0, 5.0, a)
        m.set_translation(Vector(0.0, h, 0.0, 1.0))
        self.camera = Camera(m, 1.0, 50.0)
        


def intersect(scene, ray):
    
    intersection = Intersection()
    
    for sphere in scene.spheres:
        tmpIntersection = sphere.intersect(ray)
        if tmpIntersection.intersected and tmpIntersection.ray_parameter < intersection.ray_parameter:
            intersection = tmpIntersection

    for plane in scene.planes:
        tmpIntersection = plane.intersect(ray)
        if tmpIntersection.intersected and tmpIntersection.ray_parameter < intersection.ray_parameter:
            intersection = tmpIntersection
   
       
    return intersection         
        
def trace(scene, ray, depth):
    intersection  = intersect(scene, ray)
    
    c = Color(0.0, 0.0, 0.0, 0.0)
    
    if intersection.intersected:
        c.a = 1.0
        for light in scene.lights:
            light_dir = light.position - intersection.position
            d = light_dir.length()
            attenuation = light.intensity / (d * d)
            light_dir = normalize(light_dir)
            
            shadow_ray = Ray(intersection.position + intersection.normal * 0.0001, light_dir)
            if not intersect(scene, shadow_ray).intersected:
                a = max(0.0, intersection.normal.dot(light_dir)) * attenuation
                c = Color(c.r + a, c.g + a ,c.b + a, 1.0)
        
    return c

def render_frame(i):
    img = Image(256, 256)
    scene = Scene()

    scene.set_time(float(i))
    camera = scene.camera
    
    pixel_width = 1.0 / img.width
    pixel_height = 1.0 / img.height
    
    print i
    for y in xrange(img.height):
        for x in xrange(img.width):
            img.set_pixel(x, img.height - 1 - y, Color(0.0, 0.0, 0.0, 0.0))
                
            dx = x * pixel_width - 0.5
            dy = y * pixel_height - 0.5
            
            camera_space_ray = Ray(Vector(0.0, 0.0, 0.0, 1.0), Vector(dx, dy, 1.0, 0.0))
            world_space_ray = camera.transform * camera_space_ray
            
            depth = 0
            img.set_pixel(x, img.height - 1 - y, trace(scene, world_space_ray, depth))

    img.write("img%04i.tga" % (i+1))

def main():
    map(render_frame, range(10))
    
if __name__ == "__main__":
    main()
