clear;
close;

x = [50 3065 13327 19283 23361 26409 28816 30791 32455 33887 35140 36250 ...
     37245 38145 38965 39716 40410 41053 41652 42212 42738 43233 43700 ... 
     44142 44562 44961 45341 45704 46051 46384 46703 47009 47304 47588 ...
     47862 48126 48382 48629 48868 49100];
 xs = linspace(min(x),max(x));

length = size(x,2);
 
in = csvread('inline-out-summed.csv',1,0);

gc_in = csvread('total_GC.csv',1,40);

% into seconds
y_op =  in(:,1:length) / 1e3;
y_nop = in(:,length+1:end) / 1e3;

y_op_m = mean(y_op);
y_op_e = std(y_op);

y_nop_m = mean(y_nop);
y_nop_e = std(y_nop);

[nop b] = polyfit(x,y_nop_m,1);
[op a] = polyfit(x,y_op_m,1);

[gc_fit b] = polyfit(x,gc_in(1,1:end),1);

1/(op(1)/nop(1))

linFun = @(a,xdata)( a(1)*xdata + a(2) );




p1 = errorbar(x,y_nop_m,y_nop_e);

hold on;
p2 = errorbar(x,y_op_m,y_op_e);

gcp = plot(x,gc_in(1,1:end));

gcp.Marker = 'x';

plotNOp = plot(xs,linFun(nop,xs));

plotNOp.Color = p1.Color;
p1.LineStyle = 'none';

plotOp = plot(xs,linFun(op,xs));

plotOp.Color = p2.Color;
p2.LineStyle = 'none';

plotGC = plot(xs, linFun(gc_fit,xs)); 

plotGC.Color = gcp.Color;
gcp.LineStyle = 'none';



l = legend([plotNOp plotOp plotGC], 'No Inlining', 'Inlining',...
    'GC Runtime with No Inlining','Location','northwest');
l.Box = 'off';


xlbl = xlabel('Input value $n$ to \texttt{testN}');
set(xlbl, 'interpreter', 'latex');
ylabel('Runtime/s');



matlab2tikz('../../diss/tex/evaluation/graphs/inlinePlot-out.tex',...
    'width' , '\gwidth',...
    'height', '\gheight' );
%clear 
%close
