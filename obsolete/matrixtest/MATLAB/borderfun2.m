%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Returns the difference
% ***
% *** alpha^2 * d(z,V)^2 - |z|^2
% ***
% *** between the weighted distance to the line
% ***
% *** V = [-4*kappa,0]
% ***
% *** on the negative real axis and the distance to the origin.
% ***
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function z = borderfun2(x,y,alpha,kappa,c)

if (x>=0)
    z = (alpha^2-1) * (x.^2 + y.^2);
elseif ( (x<0) && (x>-4*kappa) )
    z = alpha^2*y.^2 - (x.^2 + y.^2);
else
    z = alpha^2*((x+4*kappa).^2 + y.^2) - (x.^2+y.^2);
end