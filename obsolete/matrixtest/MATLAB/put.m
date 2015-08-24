function put(filename)

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
% shape the plot arrays
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
m = round( (Ydata(3)-Ydata(1))/Ydata(2)+1 );
n = round( (Xdata(3)-Xdata(1))/Xdata(2)+1 );

Z = zeros(m,n);

k=1;
for j=1:n
    for i=1:m
       Z(m-i+1,j) = A(k,3);
       k=k+1;
    end
end

X = [Xdata(1):(Xdata(3)-Xdata(1))/(n-1):Xdata(3)];
Y = [Ydata(1):(Ydata(3)-Ydata(1))/(m-1):Ydata(3)];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plot!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hold on;
colormap(gray)

daspect([1,1,1])

%plot axes
plot([Xdata(1) Xdata(3)],[0,0],'k')
plot([0,0],[Ydata(1) Ydata(3)],'k')
axis([Xdata(1) Xdata(3) Ydata(1) Ydata(3)])

% %plot M-matrix estimation
% fh = @(x,y) sqrt(x.^2+y.^2) - alpha*( sqrt( (x+2*kappa+c).^2+y.^2 ) - 2*kappa );
% % ezplot(@(x,y)fh(x,y),[Xdata(1) Xdata(3) Ydata(1) Ydata(3)])
% h_ez = ezcontourf(@(x,y)fh(x,y),[Xdata(1) Xdata(3) Ydata(1) Ydata(3)]);
% set(h_ez,'LineColor','none','LineStyle','none','LevelListMode','manual','LevelList',[0])
% lightgray = gray;
% lightgray(1:63,:) = 0.7;
% colormap(lightgray)

% h=get(gca,'children');
% set(h(1),'Color','red');

%  %plot phi, phi=2*arccos(1/alpha)
%  phi=2*acos(1/alpha);
%  plot([0 Xdata(1)],[0, tan(phi)*abs(Xdata(1))],'b')
%  plot([0 Xdata(1)],[0,-tan(phi)*abs(Xdata(1))],'b')

% Ctilde = alpha/2;

%  %plot guessed estimation
%  ezplot(@(x,y)borderfun2(x,y,Ctilde,kappa/2,c),[Xdata(1) Xdata(3) Ydata(1) Ydata(3)])
%  h=get(gca,'children');
%  set(h(1),'Color','blue');

%  %plot guessed estimation
%  ezplot(@(x,y)borderfun3(x,y,10000,Ctilde,kappa/2,0),[Xdata(1) Xdata(3) Ydata(1) Ydata(3)])
%  h=get(gca,'children');
%  set(h(1),'Color','blue');

%  plot([0 Xdata(1)],[0 -tan(asin(1/Ctilde))*Xdata(1)],'g')

contour(X,Y,Z,'LineStyle','-','LineColor','blue')

box on;
title('')
% hold off;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%