x = 1:35;

jhvc_in = csvread('JVHC_Fib1_35.csv',1,0);

jhvc_in = jhvc_in(:,x);

ghc_in = csvread('GHC_Fib1_35.csv',1,1);

mean_ghc = ghc_in(x,1)';

error_ghc = ghc_in(x,4)';

mean_jvhc = mean(jhvc_in) / 1e3;
error_jvhc  = std(jhvc_in) / 1e3;

nativeJava_in = csvread('nativeJava.csv',1,0) / 1e3;

mean_java = mean(nativeJava_in);
error_java = std(nativeJava_in);

fib = @(x,xdata)(x*((1+sqrt(5))/2).^xdata);

f_ja = lsqcurvefit(fib,0.2,x,mean_java)
f_gh = lsqcurvefit(fib,0.2,x,mean_ghc)
f_jv = lsqcurvefit(fib,0.2,x,mean_jvhc)

% ax = plotyy(x, mean_jv, x, mean_ghc);
% 
% l = legend('JVHC (s)', 'GHC (s)','Location','northwest'); 
% 
% set(l, 'interpreter', 'latex', 'location', 'northwest', 'FontSize', 15);
% 
% xlbl = xlabel('{Pair input number $n$}');
% 
% set(xlbl, 'interpreter', 'latex');
% 
% hold(ax(1), 'on');
% 
% 
% ylabel(ax(1), 'Type inference time in (s) for JVHC');
% 
% jv_error_bar = errorbar(ax(1), x, mean_jv,error_jv,'.');
% 
% jv_error_bar.Color = ax(1).YColor;
% 
% hold(ax(2), 'on');
% xlim(ax(1),[1.98 5.02]);
% 
% ylabel(ax(2), 'Type inference time in (s) for GHC');
% 
% jv_ghc_bar = errorbar(ax(2), x, mean_ghc,error_ghc,'.');
% 
% jv_ghc_bar.Color = ax(2).YColor;
% xlim(ax(2),[1.98 5.02]);
grey = [0.34 0.34 0.34];
ax = plotyy(x,[mean_jvhc', mean_ghc'], x, mean_java .* 1e3);

jv_line  = ax(1).Children(2);
ghc_line = ax(1).Children(1);
ja_line  = ax(2).Children(1);
ax(2).Children(1).Color = grey;
ax(2).YColor = grey;

xlbl = xlabel('Input size $n$');
set(xlbl, 'interpreter', 'latex');


l = legend('JVHC','GHC','Java','Location','northwest'); 



set(l, 'interpreter', 'latex');
 
hold(ax(1), 'on');
ylim(ax(1),[-0.05 4]);
jv_errbar = errorbar(ax(1), x,mean_jvhc,error_jvhc);
jv_errbar.Color = jv_line.Color;

ylabel(ax(1),'Runtime of Fibonacci program seconds');
 
 
ghc_errbar = errorbar(ax(1), x,mean_ghc ,error_ghc );
ghc_errbar.Color = ghc_line.Color;

hold(ax(2), 'on');
ylim(ax(2),[-0.5 40]);
ja_errbar = errorbar(ax(2), x,mean_java .* 1e3 ,error_java .* 1e3);
ja_errbar.Color = ja_line.Color;
 
ylabel(ax(2),'Runtime of Fibonacci program Java (ms)');

cleanfigure;
matlab2tikz('../../diss/tex/evaluation/graphs/plotFibRuntime-out.tex',...
    'width' , '\gwidth',...
    'height', '\gheight' );
%clear;
%close;