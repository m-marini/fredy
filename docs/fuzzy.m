tx = ty = linspace (0, 1, 33)';
[x, y] = meshgrid (tx, ty);

function z = and(x,y)
  z = min(x,y);
endfunction

function z = or(x,y)
  z = max(x,y);
endfunction

function z = xor(x,y)
  z = or(and(not(x), y), and(x, not(y)));
endfunction

function z = not(x)
  z = 1-x;
endfunction

function z = very(x)
  z = x .* x;
endfunction

function z = somewhat(x)
  z = sqrt(x);
endfunction

function z = truth(x,y)
  z = min(x, 1 - y);
endfunction

function z = falsity(x,y)
  z = isTrue(y,x);
endfunction

function z = isAntinomy(x,y)
  z = max(0, x + y - 1);
endfunction

function z = certainty(x,y)
  z = xor(x,y);
endfunction

function z = implies(x,y)
  z = or(not(x),y);
endfunction

function z = iff(x,y)
  z = or(and(x, y), and(not(x), not(y)));
endfunction

z = truth(x, y);

mesh (tx, ty, z);
xlabel ("x");
ylabel ("y");
zlabel ("z");
title ("Function");

