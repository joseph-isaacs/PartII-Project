x = 2:5;

jvhc_in = csvread('TimeExpJVHC.csv',1,1);


ghc_in = csvread('TimeExpGHC.csv',1,0) / 10e8;

y_jvhc_mean = jvhc_in(x - 1,1)';

y_jvhc_error = jvhc_in(x - 1,4)';

y_ghc_mean = mean(ghc_in);
y_ghc_error = std(ghc_in);

errorbar(x,y_jvhc_mean,y_jvhc_error);
hold on;
errorbar(x,y_ghc_mean,y_ghc_error);

legend('JVHC (s)', 'GHC (s)'); 

title

matlab2tikz('../tex/ti_plot.tex',  'width', '\fwidth' );
clear 
close