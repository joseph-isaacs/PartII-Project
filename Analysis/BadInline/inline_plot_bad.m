clear; close;

x = [50 22663 56572 73403 84193 91978 97991 102850 106903 110363 ...
     113372 116026 118396 120532 122473 124251 125887 127403 ... 
     128812 130129 131363 132524 133620 134656];
 
xs = linspace(min(x),max(x));

length = size(x,2);
 
in = csvread('out-bad-inline.csv',1,0);

% into seconds
y_op =  in(:,1:length) / 1e3;
y_nop = in(:,length+1:end) / 1e3;

mean_op = mean(y_op);
error_op = std(y_op);

mean_nop = mean(y_nop);
error_nop = std(y_nop);

p1 = errorbar(x/1e3,mean_nop,error_nop);

hold on;


p2 = errorbar(x/1e3,mean_op,error_op);

p1.Marker = 'x';

p1.MarkerSize = 5;

p2.Marker = 'x';
p2.MarkerSize = 5;

linFun = @(a,xdata)( a(1)*xdata + a(2) );

[nop a] = polyfit(x,mean_nop,1);
[op b] = polyfit(x,mean_op,1);

plotFit = plot(xs' ./ 1e3, [linFun(nop, xs)', linFun(op, xs)']);

plotFit(1).Color = p1.Color;
plotFit(2).Color = p2.Color;

p1.LineStyle = 'none';
p2.LineStyle = 'none';

l = legend(plotFit, 'No Inlining', 'Inlining', 'Location','northwest');
l.Box = 'off';

xlbl = xlabel('Input value $n$ to \texttt{badProg}/$\cdot 10^{3}$');
set(xlbl, 'interpreter', 'latex');
ylabel('Runtime/s')

cleanfigure;
matlab2tikz('../../diss/tex/evaluation/graphs/plotBadInline.tex',...
    'width' , '\gwidth',...
    'height', '\gheight' );
clear 
close
