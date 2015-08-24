function contourmov(filename)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read the data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fid = fopen(filename);

params = fscanf(fid,'%i %i',[1 2]);
Nx = params(1);
Nt = params(2);

A = fscanf(fid,'%e %e %e %e',[4,inf]);
% A = A';
fclose(fid);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Xmesh = A(3,1:Nx);
Ymesh = Xmesh;

B = zeros(Nx,Nx);

h1 = figure;

for k=1:Nt
    for j=1:Nx
        B(:,j) = A(4,(k-1)*Nx^2 + (j-1)*Nx+1:(k-1)*Nx^2 + j*Nx)';
    end

%     surf(Xmesh,Ymesh,B)
%     axis([0 1 0 1 0 0.1])
    
    contour(Xmesh,Ymesh,B,'Fill','on','LevelList',(0:0.001:1));
    caxis([0 0.09]) 
    set(gca,'XTick',[],'YTick',[])
    colormap jet
    daspect([1,1,1])
    
    if (mod(k-1,10)==0)
        print('-depsc',['../plots/img',num2str(k,'%3.0d'),'.eps'])
%         print('-dpdf', ['../plots/img',num2str(k,'%3.0d'),'.pdf']) 
    end
    

%     F(k) = getframe(gcf);
    pause(.0333)
end
% close(h1);

% h2 = figure;
% movie(h2,F,10);