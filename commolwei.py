# molar weight (g)
na2o =    61.9790
mgo =     40.3040
al2o3 =  101.9610
sio2 =    60.0840
k2o   =  94.1960
cao   = 56.0770
tio2  =  79.8660
mno   =  70.9370
feo   =  71.8440
nio   =  74.6930
zro2  = 123.2200
cl2   =  70.9060
o2    =  31.9990
h2o   =  18.0150
co2   =  44.0100

# alm Fe3Al2Si3O12
alm = feo*3 + al2o3 + sio2*3
# py   Mg3Al2Si3O12
py = mgo*3 + al2o3 + sio2*3
# gr   Ca3Al2Si3O12
gr = cao*3 + al2o3 + sio2*3

print(alm, py, gr)
