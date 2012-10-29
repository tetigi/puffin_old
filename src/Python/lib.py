# Cinesite 2011 Inspire Intern test
# =================================

import math
import copy

def mul(t, v):
    return (t[0] * v, t[1] * v, t[2] * v)

def add(lhs, rhs):
    return (lhs[0] + rhs[0], lhs[1] + rhs[1], lhs[2] + rhs[2])

def normalize(vector):
    v = copy.copy(vector)
    v /= v.length()
    return v
    
class Vector(object):
    ''' 4 dimensional vector'''
    ''' TODO add, sub, div, mul, dot, cross '''    
    def __init__(self, x, y, z, w):
        self.__m = [float(v) for v in [x, y, z, w]]
    
    def __repr__(self):
        return "Vector(%f, %f, %f, %f)" % (self.x, self.y, self.z, self.w)
     
    @property
    def x(self):
        return self.__m[0]

    @property
    def y(self):
        return self.__m[1]

    @property
    def z(self):
        return self.__m[2]

    @property
    def w(self):
        return self.__m[3]

    def __add__(self, rhs):
        if type(rhs) is Vector:
            return Vector(self.x + rhs.x, self.y + rhs.y, self.z + rhs.z, self.w + rhs.w)
        if type(rhs) is float:
            return Vector(self.x + rhs, self.y + rhs, self.z + rhs, self.w + rhs)
        
    def __sub__(self, rhs):
        if type(rhs) is Vector:
            return Vector(self.x - rhs.x, self.y - rhs.y, self.z - rhs.z, self.w - rhs.w)
        if type(rhs) is float:
            return Vector(self.x - rhs, self.y - rhs, self - rhs, self.w - rhs)
    
    def __mul__(self, rhs):
        if type(rhs) is Vector:
            return Vector(self.x * rhs.x, self.y * rhs.y, self.z * rhs.z, self.w * rhs.w)
        if type(rhs) is float:
            return Vector(self.x * rhs, self.y * rhs, self.z * rhs, self.w * rhs)
            
    def __div__(self, rhs):
        if type(rhs) is Vector:
            return Vector(self.x / rhs.x, self.y / rhs.y, self.z / rhs.z, self.w / rhs.w)
        if type(rhs) is float:
            return Vector(self.x / rhs, self.y / rhs, self.z / rhs, self.w / rhs)
        
    def dot(self, rhs):
        return self.x * rhs.x + self.y * rhs.y + self.z * rhs.z + self.w * rhs.w
        
    def cross(self, rhs):
        return Vector(self.y * rhs.z  - self.z * rhs.y,
                      self.z * rhs.x - self.x * rhs.z,
                      self.x * rhs.y - self.y * rhs.z,
                      0.0)
    
    def length(self):
        return math.sqrt(self.length_squared())
    
    def length_squared(self):
        return self.dot(self)


    def __getitem__(self, index):
        return self.__m[index]
    
    def value(self, index):
        return self.__m[index]
    
    def set_value(self, index, value):
        self.__m[index] = value
    
    def neg(self):
        return Vector(-self.__m[0], -self.__m[1], -self.__m[2], self.__m[3])
    
def tone_map(c):
    return min(int(c * 255), 255)

class Color(object):
    def __init__(self, r = 0.0, g = 0.0, b = 0.0, a = 1.0):
        self.r = r
        self.g = g
        self.b = b
        self.a = a
        
    def update(self, r, g, b, a):
        self.r = r
        self.g = g
        self.b = b
        self.a = a

    def __repr__(self):
        return "Color(%f,%f,%f,%f)" % (self.r, self.g, self.b, self.a)
    
    def __sub__(self, rhs):
        if type(rhs) is Color:
            return Color(self.r - rhs.r, self.g - rhs.g, self.b - rhs.b, self.a - rhs.a)

    def __mul__(self, rhs):
        if type(rhs) is float:
            return Color(self.r * rhs, self.g * rhs, self.b * rhs, self.a * rhs)
        if type(rhs) is Color:
            return Color(self.r * rhs.r, self.g * rhs.r, self.b * rhs.r, self.a * rhs.r)
    
    def __add__(self, rhs):
        if type(rhs) is Color:
            return Color(self.r + rhs.r, self.g + rhs.g, self.b + rhs.b, self.a + rhs.a)
        
    def sqrt(self):
        return Color(math.sqrt(self.r), math.sqrt(self.g), math.sqrt(self.b),math.sqrt(self.a))
   
class Image(object):
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.pixels = [Color()] * (width * height)
                   
    def get_size(self):
        return (self.width, self.height) 
    
    def get_pixel(self, x, y):
        return self.pixels[x + y * self.width]
    
    def set_pixel_rgba(self, x, y, r, g, b, a):
        self.pixels[x + y * self.width].update(r,g,b,a)
            
    def set_pixel(self, x, y, color):
        if (x >= 0 and x < self.width and y >=0 and y < self.height):
            self.pixels[x + y * self.width] = Color(color.r, color.g, color.b, color.a)
        
    def write(self, filename):
        pixelData = bytearray(self.width * self.height * 4);

        offsetSource = 0;
        width4 = self.width * 4;
        width8 = self.width * 8;
        offsetDest = (self.height - 1) * width4;
        for y in range(self.height):
            for x in range (self.width):
                c = self.get_pixel(x, y);
                
                pixelData[offsetDest] = tone_map(c.b)
                pixelData[offsetDest + 1] = tone_map(c.g)
                pixelData[offsetDest + 2] = tone_map(c.r)
                pixelData[offsetDest + 3] = tone_map(c.a)

                offsetSource +=1
                offsetDest += 4
            
            offsetDest -= width8
        
        header = bytearray(18)
        
        header[0] = 0 # ID length
        header[1] = 0 # no color map
        header[2] = 2 # uncompressed, true color
        header[3] = 0
        header[4] = 0
        header[5] = 0
        header[6] = 0
        header[7] = 0
        header[8] = 0 # x and y origin
        header[9] = 0
        header[10] = 0
        header[11] = 0
        header[12] = (self.width & 0x00FF)
        header[13] = ((self.width & 0xFF00) >> 8)
        header[14] = (self.height & 0x00FF)
        header[15] = ((self.height & 0xFF00) >> 8)
        header[16] = 32 # 32 bit bitmap
        header[17] = 0 
        
        f = open(filename, "wb")
        f.write(header)
        f.write(pixelData)
        f.close()
        
    def calculate_rms_error(self, other):
        if (self.width != other.width or self.height != other.height):
            return Color(-1,-1,-1,-1)
    
        rms = Color(0.0,0.0,0.0,0.0);
        for y in xrange(self.height):
            for x in xrange(self.width):
                d = self.get_pixel(x,y) - other.get_pixel(x,y);
                d2 = d * d
                sd2 = d2 * (1.0 / (self.width * self.height))
                rms = rms + sd2;
    
        return rms.sqrt();

    
    
    

    
