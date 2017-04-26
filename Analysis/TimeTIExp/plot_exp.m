clear 
close

x = 2:5;
xs = linspace(x(1),x(end));

jvhc_in = csvread('TimeExpJVHC.csv',1,1);


ghc_in = csvread('TimeExpGHC.csv',1,0) / 1e9;

mean_jv = jvhc_in(x - 1,1)';

error_jv = jvhc_in(x - 1,4)';

mean_ghc = mean(ghc_in);
error_ghc = std(ghc_in);
%plot(fi,x',fi(x));

expConst = @(a,xdata)( ((a(1) * xdata).^a(2)) + a(3));


options = optimset('MaxFunEvals',100000);
options = optimset(options, 'TolX', 1e-6);

fit_jv = lsqcurvefit(expConst,[1,2.0,0.4,2],x',mean_jv');

[fit_ghc_res, dontcare] = ghc_fit(x,mean_ghc);





[ax, line_jv, line_ghc] = plotyy(x, mean_jv, x, mean_ghc);

xlbl = xlabel('{Pair input number $n$}');

set(xlbl, 'interpreter', 'latex');

hold(ax(1), 'on');


ylabel(ax(1), 'Type inference time in for JVHC/s');

error_bar_jv = errorbar(ax(1), x', mean_jv',error_jv','x');

line_fit_jv = plot(xs',expConst(fit_jv,xs'));

line_fit_jv.Color = ax(1).YColor;

delete(line_jv)

error_bar_jv.Color = ax(1).YColor;
error_bar_jv.MarkerSize = 5;

xlim(ax(1),[1.98 5.02]);

hold(ax(2), 'on');


ylabel(ax(2), 'Type inference time for GHC/s');

error_bar_ghc = errorbar(ax(2), x', mean_ghc',error_ghc','x');

line_fit_ghc = plot(ax(2), xs',fit_ghc_res(xs'));

line_fit_ghc.Color = ax(2).YColor;

delete(line_ghc)

error_bar_ghc.Color = ax(2).YColor;
error_bar_ghc.MarkerSize = 5;
%error_bar_ghc.Marker = 'none';


xlim(ax(2),[1.98 5.02]);

h(1) = line_fit_jv;
h(2) = line_fit_ghc;

l = legend(h([1 2]), 'JVHC', 'GHC');

legend boxoff  

set(l, 'interpreter', 'latex', 'location', 'northwest', 'FontSize', 15);

cleanfigure;
matlab2tikz('../../diss/tex/evaluation/graphs/tiTime-out.tex',...
     'width' , '\gwidth',...
     'height', '\gheight' );
clear 
close