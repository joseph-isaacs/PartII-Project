x = [50,3065,13327,19283,23361,26409,28816,30791,32455,33887,35140,36250,...
    37245,38145,38965,39716,40410,41053,41652,42212,42738,43233,43700,...
    44142,44562,44961,45341,45704,46051,46384,46703,47009,47304,47588,...
    47862,48126,48382,48629,48868,49100];

length = size(x,2);

in = csvread('TimeNoInline-F.csv',1,1) .* 1e2;

in_op = csvread('TimeWithInline.csv',1,1) .* 1e2;

in_jvhc = csvread('../Inline/inline-out-summed.csv',1,0);

mean_t  = in(:,1)';
error_t = in(:,4);

mean_t_op  = in_op(:,1);
error_t_op = in_op(:,4);


y_op =  in_jvhc(:,1:length) / 1e3;
y_nop = in_jvhc(:,length+1:end) / 1e3;

y_op_m = mean(y_op);
y_op_e = std(y_op);

y_nop_m = mean(y_nop);
y_nop_e = std(y_nop);



gp1 = errorbar(x,mean_t,error_t);
gp1.Bar.LineStyle = 'dotted';
hold on;
gp2 = errorbar(x,mean_t_op,error_t_op);
gp2.Bar.LineStyle = 'dotted';


p1 = errorbar(x,y_op_m,y_op_e);
p1.Bar.LineStyle = 'dotted';
p2 = errorbar(x,y_nop_m,y_nop_e);
p2.Bar.LineStyle = 'dotted'; 

legend('GHC No Op 10^{-2}','GHC Op 10^{-2}','JVHC Op','JVHC No Op'...
    ,'Location','northwest');

xlabel ('Input size (n)')
ylabel ('Time to run sec')


matlab2tikz('../tex/ghc_inlining.tex',  'width', '\fwidth' );
clear 
close