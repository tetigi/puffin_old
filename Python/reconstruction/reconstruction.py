# Cinesite 2011 Inspire Intern test
# =================================

import img1
import img2
import img3

import sys
sys.path.append("..")
from lib import *

def make_image(img):
    w = img[0]
    h = img[1]
    arr = bytearray(img[2].decode("hex"), "hex")
    
    image = Image(w, h)
    
    for y in xrange(h):
        for x in xrange(w):    
            r = arr[4 * (x + y * w) + 0] / 255.0 
            g = arr[4 * (x + y * w) + 1] / 255.0
            b = arr[4 * (x + y * w) + 2] / 255.0
            a = arr[4 * (x + y * w) + 3] / 255.0
            
            image.set_pixel(x, y, Color(r, g, b, a))
            
    return image


class TestData(object):
    def __init__(self):
        self.__images = [None, None, None]
        self.__data = [img1.img1, img2.img2, img3.img3]
        self.__max_samples = [0,0,0]
        self.__samples = [0,0,0]
        
    def __init__image(self, index):
        if self.__images[index] == None:
            self.__images[index] = make_image(self.__data[index])
            self.__max_samples = self.num_samples(index)
            
    def get_size(self, index):
        self.__init__image(index)
        return self.__images[index].get_size()
    
    def num_samples(self, index):
        width, height = self.get_size(index)
        return int(0.25 * width * height)
    
    def sample(self, index, x, y):
    
        self.__samples[index] += 1

        if self.__samples[index] <= self.__max_samples:
            self.__init__image(index)
            return self.__images[index].get_pixel(x,y)
        else:
            return Color(0,0,0,0);
    
    def calculate_error(self, index, image):
        return self.__images[index].calculate_rms_error(image);


def main():
    d = TestData()
    width, height = d.get_size(0)
    
    out = Image(width, height)
    for x in xrange(width):
        for y in xrange(height):

            out.set_pixel(x, y, d.sample(0, x, y))
    
    c = d.calculate_error(0, out)
    print c        
    out.write("reconstruction-test-py.tga")


if __name__ == "__main__":
    main()
