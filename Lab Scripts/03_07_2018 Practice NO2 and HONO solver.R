a = 23556.4091
b = 15012.94288
c = 2600
d = 0.588 
e = 0.751
f = 0.0966
g = 0.073 

da = 98
db = 158
dd = 0.005
de = 0.010
df = 0.0028
dg = 0.0040

NO2 = (1/c*(b - g/f * a))/(e-g*d/f)
HONO = (1/c*(b - a*e/d))/(g-e*f/d)

Da = g/(c*(e*f-d*g)) * da
Db = f/(c*(e*f-d*g)) * db
Dd = g*(b*f-a*g)/(c*(e*f-g*d)^2) * dd
De = f*(b*f-a*g)/(c*(e*f-d*g)^2) * de
Df = g*(e*a-b*d)/(c*(e*f-d*g)^2) * df
Dg = (-e*a*f+b*d*f)/(c*(e*f-d*g)^2) * dg

NO2sd = sqrt(Da^2 + Db^2 + Dd^2 + De^2 + Df^2 + Dg^2)


