data=load('Z:\Home\Develop\hopes_dev\src\Qualicontr\P4.txt');
x=data(:,1);
y=data(:,2);
z=data(:,3)-mean(data(:,3));
% z=data(:,3);
% snr = data(:,4);
% subplot(3,1,1),plot( x(170:280),y(170:280),'r.');
% subplot(3,1,2),plot( x(170:280),z(170:280),'g.');
% 
% subplot(3,1,3),plot(x(170:280),20*log10(snr(170:280)/sqrt(2)),'b.');

%
xx=(x(:)-mean(x))/std(x);
order=4
% plot(xx,y);
hold on;
length(x);
for i=1:length(x)
    for j=1:order+1
        if j==1
            A(i,j) =1;
        else
            A(i,j) = xx(i)^(j-1);
        end
    end
end
N=length(x)
for  i=1:N
    for j=1:order+1
        AT(j,i) = A(i,j);
    end
end
Nbb=zeros(order+1,order+1);
w=zeros(order+1,1)
if(N>order)
    for i=1:order+1
        for j=1:order+1
            for k=1:N
                Nbb(i,j) = Nbb(i,j) + AT(i,k)*A(k,j);
            end
        end
    end
    for i=1:order+1
        for j=1:N
            w(i) = w(i) + AT(i,j)*y(j);
        end
    end
end

coeff= inv(Nbb)*w

coeff = A\y
yy=[];
for i=1:length(x)
    for j=1:order+1
        if(j==1)
            yy(i) = coeff(1);
        else
            yy(i) = yy(i) + coeff(j)*xx(i)^(j-1);
        end
    end
end
hold on;

plot(xx*std(x),y,'b.-',xx*std(x),yy,'r-');
hold on
plot(xx*std(x),z,'g.')

