%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** FUNCTION SHOWV
% ***
% *** This routine shows an approximation of the set V as apperearing in the
% *** bound
% ***
% *** 1 / dist(z,V) \le C / |z|
% ***
% *** based on the output given by MTEST.F90.
% ***
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function showV(filename)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read the data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fid = fopen(filename);

params = fscanf(fid,'%e %e %e',[3,1]);
alpha = params(1);
kappa = params(2);
c     = params(3);

Xdata = fscanf(fid,'%e %e %e',[3,1]);

Ydata = fscanf(fid,'%e %e %e',[3,1]);

A = fscanf(fid,'%e %e %e',[3,inf]);
A = A';

fclose(fid);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% open a window
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fig1 = figure;
whitebg('black');
daspect([1,1,1])
hold on;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plot the circles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
N = size(A,1);

NOP = 100;
for k=2:N-1

    if ( or(A(k,3)==0,and(A(k+1,3)==1,A(k-1,3)==1)) )
        continue
    end

    r = sqrt(A(k,1:2)*A(k,1:2)') / (alpha/1.6);

    %plot circle
    THETA=linspace(0,2*pi,NOP);
    RHO=ones(1,NOP)*r;
    [X,Y] = pol2cart(THETA,RHO);
    X=X+A(k,1);
    Y=Y+A(k,2);
%      H=fill(X,Y,'w','LineStyle','none');
    plot(X,Y,'w');

end

hold off;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%