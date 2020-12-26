import matplotlib.pyplot as plt
from matplotlib.ticker import FormatStrFormatter
from matplotlib.ticker import AutoMinorLocator
plt.figure(figsize=(8.55,7.55))

x = [i for i in range(0,9)]
y = [1.333 + i * .0043/2 - .00001 for i in range(0,9)]


plt.xticks(x)
plt.yticks([1.333 + i * 0.002 for i in range(0,9)])
#plt.gca().yaxis.set_major_formatter(FormatStrFormatter('%.4f'))

#plt.yticks([0.3 + i * .1 for i in range(-4,33) if i % 2 == 0])

plt.xlabel('C', ha='right', va='top', x=1, fontsize=20)
plt.ylabel('n', va='top', y=1.05, x=9.0, rotation=0, fontsize=20)

plt.minorticks_on()
plt.gca().xaxis.set_minor_locator(AutoMinorLocator(5))
plt.gca().yaxis.set_minor_locator(AutoMinorLocator(5))
plt.grid(which='major', linestyle='-', linewidth='1.0', color='darkcyan')
plt.grid(which='minor', linestyle='-', linewidth='0.6', color='darkcyan')

#plt.plot( [0,0], [-.1,3.5], ':', linewidth = .6, color='c' )
#plt.plot( [-10,310] ,[0,0], ':', linewidth = .6, color='c' )

plt.plot(x, y, lw=4,color='m')

ux = 3
uy = 1.3394

inters_col = 'darkorange'
plt.plot( [ux,ux] ,[1.332,uy],   '--', linewidth = 3.5, color=inters_col )
plt.plot( [-1,ux] ,[uy,uy], '--', linewidth = 3.5, color=inters_col )
plt.plot( [ux],[uy], marker='x', markersize = 14.5, markeredgewidth=5.0, color='black' )
plt.plot( [ux],[uy], marker='x', markersize = 12.5, markeredgewidth=2, color=inters_col )

plt.xlim([0,8])
plt.ylim([1.333, 1.349])#1 * 1.349 + .001 ])

plt.show()

