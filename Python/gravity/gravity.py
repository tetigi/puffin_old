# Cinesite 2011 Inspire Intern test
# =================================

import copy
import random
import sys
sys.path.append("..")
from lib import *

class InitialConditions(object):
    '''
    Class which defines the initial state of the particles
    '''
    def __init__(self, num_objects):
        self.__num_objects = num_objects
        self.__initial_pos = self.__get_initial_positions()
        self.__initial_vel = self.__get_initial_velocties()
        self.__masses = self.__get_masses()
        
    @property
    def positions(self):
        return self.__initial_pos
    
    @property
    def velocities(self):
        return self.__initial_vel

    @property
    def masses(self):
        return self.__masses
     
    def __get_initial_positions(self):
        centres = ((4e2, 5e2, 0.0), (7e2,8e2,0.0))
        radii = ((1e2,1e2,0.5e2), (1e2,0.5e2,1e2))
        
        positions = []
        for g in range(2):
            centre = centres[g]
            for s in xrange(self.__num_objects):
                p = (random.normalvariate(centres[g][0], radii[g][0]),
                     random.normalvariate(centres[g][0], radii[g][1]), 
                     random.normalvariate(centres[g][0], radii[g][2]))
                positions.append(p)
        return positions 
    
    def __get_initial_velocties(self):
        velocities = []
        for g in range(2):
            if g == 0:
                v = (0.0, 0.0, 0.0)
            else:
                v = (0.0, 0.0, 0.0)
            
            for s in xrange(self.__num_objects):
                velocities.append(v)
            
        return velocities
    
    def __get_masses(self):
        masses = []
        for g in range(2):
            for s in xrange(self.__num_objects):
                m = random.normalvariate(2000.0, 500.0)
                masses.append(m)
                
        return masses
        

def main():
    # create the initial state of the system
    # 128k x 2 masses
    num_particles = 128 * 1024
    ic  = InitialConditions(num_particles)
        
    #image to render into
    img = Image(1024, 1024)
    
    #render orthographic projection of masses to image
    for p in ic.positions:
        x = int(p[0])
        y = int(p[1])
        
        c = img.get_pixel(x, y)
        img.set_pixel(x, y, Color(0.2 + c.r, 0.2 + c.g, 0.2 + c.b, 1.0))
        
    img.write("gravity-test-py.tga")

if __name__ == "__main__":
    main()
