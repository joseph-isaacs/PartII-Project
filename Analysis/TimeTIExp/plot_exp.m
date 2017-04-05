x = 2:5;

jvhc_in = csvread('TimeExpJVHC.csv',1,1);


ghc_in = csvread('TimeExpGHC.csv',1,0) / 1e9;

mean_jv = jvhc_in(x - 1,1)';

error_jv = jvhc_in(x - 1,4)';

mean_ghc = mean(ghc_in);
error_ghc = std(ghc_in);



ax = plotyy(x, mean_jv, x, mean_ghc);

l = legend('JVHC (s)', 'GHC (s)','Location','northwest'); 

set(l, 'interpreter', 'latex', 'location', 'northwest', 'FontSize', 15);

xlbl = xlabel('{Pair input number $n$}');

set(xlbl, 'interpreter', 'latex');

hold(ax(1), 'on');


ylabel(ax(1), 'Type inference time in (s) for JVHC');

jv_error_bar = errorbar(ax(1), x, mean_jv,error_jv,'.');

jv_error_bar.Color = ax(1).YColor;

hold(ax(2), 'on');
xlim(ax(1),[1.98 5.02]);

ylabel(ax(2), 'Type inference time in (s) for GHC');

jv_ghc_bar = errorbar(ax(2), x, mean_ghc,error_ghc,'.');

jv_ghc_bar.Color = ax(2).YColor;
xlim(ax(2),[1.98 5.02]);





cleanfigure;
%matlab2tikz('../../diss/tex/evaluation/graphs/tiTime-out.tex',...
%     'width' , '\gwidth',...
%     'height', '\gheight' );
%clear 
%close