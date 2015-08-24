%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** Returns the difference
% ***
% *** alpha^2 * d(z,V)^2 - |z|^2
% ***
% *** between the weighted distance to the "shell" defined by the two circles
% *** centered at
% ***
% *** (x0,y0), (x0,-y0)
% ***
% *** with given radii r with (x0,y0) such that the circles meet at
% ***
% *** (-4*kappa,0) and (0,0) 
% *** and the distance to the origin.
% ***
% ***
% ***  With a little        | Im-axis
% ***  imagination:         |
% ***  The set V            |
% ***          _________    |
% ***      ___/         \___|
% ***     /                \|
% *** ------------------------------------- Re-axis
% ***     \___          ___/|
% ***         \________/    |
% ***                       |
% ***                       |
% ***                       |
% ***                       |
% ***
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function z = borderfun3(x,y,r,alpha,kappa,c)

if (r<=2*kappa)
   stop;
end

x0 = -2*kappa;
y0 = sqrt(r^2-x0^2);

if (y==0)

    if(x>=0)
        z = (alpha^2-1)* x^2;
    elseif (x<=-4*kappa)
        z = alpha^2*(x+4*kappa)^2 - x^2;
    else
        z = alpha^2 * ( r - sqrt((x+2*kappa)^2+y0^2) )^2 - x^2;
    end

else

    if ( x>=0 && abs(x/y)>=abs(x0/y0) ) %dist(z,V) = |z|
        z = (alpha^2-1) * (x.^2 + y.^2);
    elseif ( x<=-4*kappa && abs((x+4*kappa)/y)>=abs(x0/y0) ) %dist(z,V) = |z+4*kappa|
        z = alpha^2 * ((x+4*kappa).^2 + y.^2) - (x.^2 + y.^2);
    else
        z = alpha^2 * ( sqrt((x-x0)^2 + (y+sign(y)*y0)^2) - r )^2 - (x.^2+y.^2);
    end

end