import matplotlib.pyplot as plt
from matplotlib.patches import Circle
import numpy as np
import math
from mpl_toolkits import mplot3d
from Bezier import Bezier
from numpy import array as a
from mpl_toolkits.mplot3d import Axes3D
import mpl_toolkits.mplot3d.art3d as art3d

#Procedimientos para el calculo del área superficial
def Area_halfsphere(r):
  h = r
  area = 2 * math.pi * r * h
  return area

def Area_pyramid(l):
  area = (l*l/2)*3
  return area

def Area_mortero():
  area = Area_halfsphere(1.00) + 4 * Area_pyramid(0.30)
  return area

#Procedimientos para el volumen superficial
def Volume_halfsphere(r):
  volume = (4/3 * math.pi * r**3)/2
  return(volume)

def Volume_pyramid(l):
  h = (l**2+(l/2)**2)**(1/2)
  b = l*l/2
  volume = 1/3 * b * h
  return(volume)

def Volume_mortero():
  volume = (Volume_pyramid(0.30) -  Volume_pyramid(0.29)) * 4 + (Volume_halfsphere(1.00) - Volume_halfsphere(0.99))
  return(volume)


fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

h = -1 #Altura (z)
b = 0.999999999 #Radio 2
cg = 0
c = 0.55191502449 #constante para mostrar un circulo

#Vaso del mortero (cuerpo)
for i in range(0, 65):
	if i == 0:
		p = Circle((0, 0), b, color="green", fill=True)
	else:
		p = Circle((0, 0), b, color="green", fill=False)
	ax.add_patch(p)
	art3d.pathpatch_2d_to_3d(p, z = h)

	if i<5:
		b = b + 0.1
	elif i<10:
		b = b - 0.1
	elif i<30:
		b = b + 0.1
	elif i<55:
		b= b
	elif i>55:
		b = b -0.1

	h = h + 0.02
h = h - (0.02)*5
#Generar con curvas de Bezier la salida del mortero
x = 0
for i in range(0, 12):
	points_set_1 = a([[-b, -2.1, h], [-1, -2.2 - x, h], [1, -2.2 - x, h], [b, -2.1, h]])
	t_points = np.arange(0, 1, 0.01)
	curve_set_1 = Bezier.Curve(t_points, points_set_1)
	ax.plot(curve_set_1[:, 0], curve_set_1[:, 1], curve_set_1[:, 2], color="green")

	points_set_2 = a([[-b, 2.1, h], [-1, 3.2, h], [1, 3.2, h], [b, 2.1, h]])
	t_points = np.arange(0, 1, 0.01)
	curve_set_2 = Bezier.Curve(t_points, points_set_2)
	ax.plot(curve_set_2[:, 0], curve_set_2[:, 1], curve_set_2[:, 2], color="green")

	points_set_3 = a([[2.1, b, h], [3.2, 1, h], [3.2, -1, h], [2.1, -b, h]])
	t_points = np.arange(0, 1, 0.01)
	curve_set_3 = Bezier.Curve(t_points, points_set_3)
	ax.plot(curve_set_3[:, 0], curve_set_3[:, 1], curve_set_3[:, 2], color="green")

	points_set_4 = a([[-2.1, -b, h], [-3.2, -1, h], [-3.2, 1, h], [-2.1, b, h]])
	t_points = np.arange(0, 1, 0.01)
	curve_set_4 = Bezier.Curve(t_points, points_set_4)
	ax.plot(curve_set_4[:, 0], curve_set_4[:, 1], curve_set_4[:, 2], color="green")

	h = h + 0.03
	x = x + 0.3


#Llamar función para calcular área del mortero
print("El area superficial del mortero es: ",Area_mortero())
print("El volumen superficial del mortero es: ",Volume_mortero())

ax.set_xlim(-5, 5)
ax.set_ylim(-5, 5)
ax.set_zlim(-2, 2)
plt.show()
