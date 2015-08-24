function contourmov(filename)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read the data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fid = fopen(filename);

params = fscanf(fid,'%i %i',[1 2]);
Nx = params(1);
Nt = params(2);

A = fscanf(fid,'%e %e %e',[3,inf]);
% A = A';
fclose(fid);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x0 = min(A(2,:));
x1 = max(A(2,:));

y0 = 0; %min(A(3,:));
y1 = 1.1*max(abs(A(3,:)));

h1 = figure;
N = Nx + 2; %number of points, including both boundary points
for k=1:Nt+1

%      plot(A(2,(k-1)*N+1:k*N),abs(A(3,(k-1)*N+1:k*N)),'r','LineWidth',3);
    plot(A(2,(k-1)*N+1:k*N),abs(A(3,(k-1)*N+1:k*N)),...
         '^','LineStyle','none','MarkerEdgeColor','r','MarkerFaceColor','r','MarkerSize',5);

    axis([x0 x1 y0 y1])
%      daspect([1,1,1])

    if (mod(k-1,20)==0)
%          print('-depsc',['../plots/error',num2str(k-1,'%3.0d'),'.eps'])
        print('-dpdf', ['../plots/error',num2str(k-1,'%3.0d'),'.pdf'])
    end


%     F(k) = getframe(gcf);
    pause(.1)
end
% close(h1);

% h2 = figure;
% movie(h2,F,10);