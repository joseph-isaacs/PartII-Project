x = [50 3065 13327 19283 23361 26409 28816 30791 32455 33887 35140 36250 ...
     37245 38145 38965 39716 40410 41053 41652 42212 42738 43233 43700 ... 
     44142 44562 44961 45341 45704 46051 46384 46703 47009 47304 47588 ...
     47862 48126 48382 48629 48868 49100];

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

[op a] = polyfit(x,y_op_m,1);
[nop a] = polyfit(x,y_nop_m,1);

1/(op(1)/nop(1))







p1 = errorbar(x,y_op_m,y_op_e);
hold on;
p2 = errorbar(x,y_nop_m,y_nop_e);

gcp = plot(x,gc_in);

l = legend([p1 p2 gcp(2)], 'Inlined', 'No Inlined','GC Runtime','Location','northwest');
l.Box = 'off';


xlbl = xlabel('Input value $n$ to \texttt{testN}');
set(xlbl, 'interpreter', 'latex');
ylabel('Runtime in seconds');



%matlab2tikz('../../diss/tex/evaluation/graphs/inlinePlot.tex',...
%    'width' , '\gwidth',...
%    'height', '\gheight' );
%clear 
%close
