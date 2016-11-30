from numpy import *


def get_R_o(EAngles):

    # returns rotation matrix
            
    R_o = zeros((3,3),dtype=double)
    
    euler_phi1 = EAngles[0]
    euler_Phi = EAngles[1]
    euler_phi2 = EAngles[2]
    
    R_o[0,0]= cos(euler_phi1)*cos(euler_phi2) - sin(euler_phi1)*sin(euler_phi2)*cos(euler_Phi)
    R_o[1,0]= -cos(euler_phi1)*sin(euler_phi2) - sin(euler_phi1)*cos(euler_phi2)*cos(euler_Phi)
    R_o[2,0]= sin(euler_phi1)*sin(euler_Phi)
    R_o[0,1]= sin(euler_phi1)*cos(euler_phi2) + cos(euler_phi1)*sin(euler_phi2)*cos(euler_Phi)
    R_o[1,1]= -sin(euler_phi1)*sin(euler_phi2) + cos(euler_phi1)*cos(euler_phi2)*cos(euler_Phi) 
    R_o[2,1]= -cos(euler_phi1)*sin(euler_Phi)
    R_o[0,2]= sin(euler_phi2)*sin(euler_Phi)  
    R_o[1,2]= cos(euler_phi2)*sin(euler_Phi)
    R_o[2,2]= cos(euler_Phi) 

    return asmatrix(R_o)


import numpy as np
from math import sqrt


def bcrossn():
    b = np.array([[0, 1, -1],
                 [1, 0, -1],
                 [1, -1, 0],
                 [0, -1, 1],
                 [1, 0, 1],
                 [1, 1, 0],
                 [0, 1, 1],
                 [-1, 0, 1],
                 [-1, -1, 0],
                 [0, -1, -1],
                 [-1, 0, -1],
                 [-1, 1, 0]])
    
    n = np.array([[1, 1, 1],
                 [1, 1, 1],
                 [1, 1, 1],
                 [-1, 1, 1],
                 [-1, 1, 1],
                 [-1, 1, 1],
                 [1, -1, 1],
                 [1, -1, 1],
                 [1, -1, 1],
                 [1, 1, -1],
                 [1, 1, -1],
                 [1, 1, -1]])
    
    m = np.empty((12,3,3))
    q = np.empty((12,3,3))
    for i in range(12):
        Schmid = ((1 / sqrt(6)) * b[i,:].reshape((3,1)) * n[i,:])
        m[i,:,:] = .5 * (Schmid + Schmid.T)
        q[i,:,:] = .5 * (Schmid - Schmid.T)
    return m,q


def vertex_stress(i):
    v = np.zeros((28,6))
    v[0] = [1, -1, 0, 0, 0, 0]
    v[1] = [0, 1, -1, 0, 0, 0]
    v[2] = [-1, 0, 1, 0, 0, 0]
    v[3] = [0, 0, 0, 1, 0, 0]
    v[4] = [0, 0, 0, 0, 1, 0]
    v[5] = [0, 0, 0, 0, 0, 1]
    v[6] = [.5, -1, .5, 0, .5, 0]
    v[7] = [.5, -1, .5, 0, -.5, 0]
    v[8] = [-1, .5, .5, .5, 0, 0]
    v[9]= [-1, .5, .5, -.5, 0, 0]
    v[10]= [.5, .5, -1, 0, 0, .5]
    v[11]= [.5, .5, -1, 0, 0, -.5]
    v[12]= [.5, 0, -.5, .5, 0, .5]
    v[13]= [.5, 0, -.5, -.5, 0, .5]
    v[14]= [.5, 0, -.5, .5, 0, -.5]
    v[15]= [.5, 0, -.5, -.5, 0, -.5]
    v[16]= [0, -.5, .5, 0, .5, .5]
    v[17]= [0, -.5, .5, 0, -.5, .5]
    v[18]= [0, -.5, .5, 0, .5, -.5]
    v[19]= [0, -.5, .5, 0, -.5, -.5]
    v[20]= [-.5, .5, 0, .5, .5, 0]
    v[21]= [-.5, .5, 0, -.5, .5, 0]
    v[22]= [-.5, .5, 0, .5, -.5, 0]
    v[23]= [-.5, .5, 0, -.5, -.5, 0]
    v[24]= [0, 0, 0, .5, .5, .5]
    v[25]= [0, 0, 0, .5, -.5, .5]
    v[26]= [0, 0, 0, -.5, .5, .5]
    v[27]= [0, 0, 0, .5, .5, .5]

    return sqrt(6) * v[i]


def vect2dev_tensor(V):
    DT = np.zeros((3,3))
    DT[0,0] = (V[2] - V[1]) / 3
    DT[1,1] = (V[0] - V[2]) / 3
    DT[2,2] = (V[1] - V[0]) / 3
    DT[0,1] = V[5]
    DT[1,0] = V[5]
    DT[0,2] = V[4]
    DT[2,0] = V[4]
    DT[1,2] = V[3]
    DT[2,1] = V[3]
    return DT


def BHmodel(de_c, g, i):
    dW = []
    for j in range(28):
        vs = vertex_stress(j)
        A = vs[0]
        B = vs[1]
        C = vs[2]
        F = vs[3]
        G = vs[4]
        H = vs[5]

        dW.append(-B * de_c[0,0] + A * de_c[1,1] + 2 * F * de_c[2,1] + 2 * G * de_c[2,0] + 2 * H * de_c[0,1])
    
    maxdW = 0
    index = -1
    for i in range(len(dW)):
        if abs(dW[i]) > maxdW:
            maxdW = abs(dW[i])
            index = i

    if dW[index] < 0:
        S_c = -1 * vertex_stress(index)
    else:
        S_c = vertex_stress(index)

    Stensor_c = vect2dev_tensor(S_c)
    return g.T * Stensor_c * g


def get_taylor(phi1,Phi,phi2):
    Bunge = np.array((phi1, Phi, phi2)).T
    
### STRAIN TENSOR? ###

    de = np.array([[-.5,  0,   0],
                  [0, -.5,  0],
                  [0,  0,  1]])

 ### STRAIN TENSOR? ###
    
    m, q = bcrossn()
    S = np.zeros((3,3))
    Sdotde = 0
    dedotde = 0
    eVM = 0
    g = get_R_o(Bunge).T
    de_c = g * de * g.T

    Sc = BHmodel(de_c, g, 0)
    # print Sc
    Sdotde = 0
    dedotde = 0
    eVM = 0

    for k in range(3):
        for l in range(3):
            Sdotde += Sc[k,l] * de[k,l]
            dedotde += de[k,l] * de[k,l]

    eVM = sqrt((2.0 / 3.0) * dedotde)
    M = Sdotde / eVM
    return M