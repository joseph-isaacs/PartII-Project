x = [50 22663 56572 73403 84193 91978 97991 102850 106903 110363 ...
     113372 116026 118396 120532 122473 124251 125887 127403 ... 
     128812 130129 131363 132524 133620 134656];

length = size(x,2);
 
in = csvread('out-bad-inline.csv',1,0);

% into seconds
y_op =  in(:,1:length) / 1e3;
y_nop = in(:,length+1:end) / 1e3;

mean_op = mean(y_op);
error_op = std(y_op);

mean_nop = mean(y_nop);
error_nop = std(y_nop);


p1 = errorbar(x/1e3,mean_op,error_op);
hold on;
errorbar(x/1e3,mean_nop,error_nop);


l = legend('With Inlining', 'Without Inlining', 'Location','northwest');
l.Box = 'off';

xlbl = xlabel('Input value $n$ in 1000 to \texttt{badProg}');
set(xlbl, 'interpreter', 'latex');
ylabel('Runtime in seconds')

cleanfigure;
matlab2tikz('../../diss/tex/evaluation/graphs/plotBadInline.tex',...
    'width' , '\gwidth',...
    'height', '\gheight' );
clear 
close
