function z = borderfun(x,y,alpha,kappa,c)

z = sqrt(x.^2+y.^2) - alpha*( sqrt( (x+2*kappa+c).^2+y.^2 ) - 2*kappa );