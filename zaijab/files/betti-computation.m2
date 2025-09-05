-- Compute Betti table for the requested ideal
R = QQ[x,y,z];
I = ideal(x^3*y, x^2*y^2, x*y^3, y^4, z^5);
print("Ideal I = ");
print(toString gens I);
print("");
print("Computing resolution...");
C = res I;
print("Resolution computed!");
print("");
print("BETTI TABLE:");
print(betti C);
print("");
print("Detailed resolution information:");
print("Length of resolution: " | toString length C);
for i from 0 to length C do (
    print("C_" | toString i | " has rank " | toString rank C_i);
);