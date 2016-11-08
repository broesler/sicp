%===============================================================================
%     File: fibonacci.m
%  Created: 11/01/2016, 21:24
%   Author: Bernie Roesler
%
%  Description: Plot fibonacci numbers vs phi^n/sqrt(5) to show convergence.
%
%===============================================================================
clear; clearfigs();

plot_flag = 1;

n = 50;
n_vec = (0:n)';

% initialize 
fib   = zeros(n,1);
fib(1:2) = [0 1];

for i = 3:n+1
    fib(i) = fib(i-1) + fib(i-2);
end

phi = (1 + sqrt(5)) / 2; % golden ratio
phi_n = phi.^n_vec / sqrt(5);

% print first values
for i = 1:8
    fprintf('%8.4f %8.4f\n', fib(i), phi_n(i));
end

% calculate error
err = abs(phi_n - fib) ./ fib;

% plots
if plot_flag == 1
    figure(1);
    hf = semilogy(n_vec, fib,'-x');
    hold on; grid on; box on;
    hp = semilogy(n_vec, phi_n,'-o');

    ll = legend([hf hp], 'F(n) = F(n-1) + F(n-2)', '$\frac{\phi^n}{\sqrt{5}}$');
    set(ll, 'Location', 'SouthEast', 'interpreter', 'latex');

    figure(2)
    he = semilogy(n_vec, err, '-x');
    hold on; grid on; box on;
    title('Relative Error')
end
%===============================================================================
%===============================================================================
