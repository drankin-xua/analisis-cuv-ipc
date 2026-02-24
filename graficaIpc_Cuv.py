import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import numpy as np
import matplotlib.dates as mdates

# Cargar datos
df = pd.read_csv("C:/Users/drankin/Documents/Base_limpia_auto_energía/datos_graf.csv")
df['fecha'] = pd.to_datetime(df['fecha'])
df = df.sort_values('fecha').reset_index(drop=True)

# Calcular variacion porcentual sobre serie completa
df['var_ipc'] = df['IPC'].pct_change() * 100
df['var_cuv'] = df['CUV'].pct_change() * 100

# Estilo academico
plt.rcParams.update({
    'font.family': 'serif',
    'font.size': 11,
    'axes.spines.top': False,
    'axes.spines.right': False,
    'figure.dpi': 150
})

color_ipc = '#1a4f8a'
color_cuv = '#8b0000'

def formato_fecha(x, pos):
    fecha = mdates.num2date(x)
    if fecha.month == 1:
        return fecha.strftime('%b\n%Y')
    else:
        return fecha.strftime('%b')

FUENTE = 'Fuente: DANE y Superintendencia de Servicios Públicos Domiciliarios (SSPD)'

# ============================================================
# GRAFICA 1: Series en niveles (doble eje)
# ============================================================
df_niv = df[df['fecha'] >= '2020-01-01'].dropna(subset=['IPC', 'CUV'])

fig, ax1 = plt.subplots(figsize=(18, 5))
ax2 = ax1.twinx()

ax1.plot(df_niv['fecha'], df_niv['IPC'], color=color_ipc, linewidth=1.8,
         label='IPC de Electricidad')
ax2.plot(df_niv['fecha'], df_niv['CUV'], color=color_cuv, linewidth=1.8,
         linestyle='--', label='CUV Promedio Nacional')

ax1.set_ylabel('IPC de Electricidad (dic-2018 = 100)', color=color_ipc, fontsize=11)
ax2.set_ylabel('CUV ($/kWh)', color=color_cuv, fontsize=11)
ax1.tick_params(axis='y', labelcolor=color_ipc)
ax2.tick_params(axis='y', labelcolor=color_cuv)

ax1.set_title('IPC de Electricidad y Costo Variable Unitario Promedio Nacional',
              fontweight='bold', fontsize=13, loc='left')

lines1, labels1 = ax1.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax1.legend(lines1 + lines2, labels1 + labels2,
           loc='upper left', frameon=False, fontsize=10)

ax1.grid(axis='y', linestyle='--', alpha=0.4, color='gray')
ax1.spines['top'].set_visible(False)
ax2.spines['top'].set_visible(False)

ax1.xaxis.set_major_locator(mdates.MonthLocator(bymonth=[1, 4, 7, 10]))
ax1.xaxis.set_major_formatter(mticker.FuncFormatter(formato_fecha))
plt.setp(ax1.xaxis.get_majorticklabels(), fontsize=9, rotation=0)

ax1.set_xlim(df_niv['fecha'].min() - pd.Timedelta(days=15),
             df_niv['fecha'].max() + pd.Timedelta(days=15))

fig.text(0, -0.02, FUENTE, fontsize=9, color='gray')
plt.tight_layout()
plt.savefig("C:/Users/drankin/Documents/Base_limpia_auto_energía/grafica_niveles.png",
            dpi=300, bbox_inches='tight')
plt.show()

# ============================================================
# GRAFICA 2: Variacion porcentual (doble eje)
# ============================================================
df_var = df[df['fecha'] >= '2020-02-01'].dropna(subset=['var_ipc', 'var_cuv'])

fig, ax1 = plt.subplots(figsize=(18, 5))
ax2 = ax1.twinx()

ax1.plot(df_var['fecha'], df_var['var_ipc'],
         color=color_ipc, linewidth=1.4,
         marker='o', markersize=2.5, markerfacecolor=color_ipc,
         label='Variación IPC (%)')
ax1.axhline(0, color='black', linewidth=0.5, linestyle='-')

ax2.plot(df_var['fecha'], df_var['var_cuv'],
         color=color_cuv, linewidth=1.4,
         marker='s', markersize=2.5, markerfacecolor=color_cuv,
         linestyle='--', label='Variación CUV (%)')

ax1.set_ylabel('Variación IPC (%)', color=color_ipc, fontsize=11)
ax2.set_ylabel('Variación CUV (%)', color=color_cuv, fontsize=11)
ax1.tick_params(axis='y', labelcolor=color_ipc)
ax2.tick_params(axis='y', labelcolor=color_cuv)

ax1.set_title('Variación Porcentual Mensual del IPC de Electricidad y del CUV',
              fontweight='bold', fontsize=13, loc='left')

lines1, labels1 = ax1.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax1.legend(lines1 + lines2, labels1 + labels2,
           loc='upper left', frameon=False, fontsize=10)

ax1.grid(axis='y', linestyle='--', alpha=0.4, color='gray')
ax1.grid(axis='x', linestyle=':', alpha=0.3, color='gray')
ax1.spines['top'].set_visible(False)
ax2.spines['top'].set_visible(False)

ax1.xaxis.set_major_locator(mdates.MonthLocator(bymonth=[1, 4, 7, 10]))
ax1.xaxis.set_major_formatter(mticker.FuncFormatter(formato_fecha))
plt.setp(ax1.xaxis.get_majorticklabels(), fontsize=9, rotation=0)

ax1.set_xlim(df_var['fecha'].min() - pd.Timedelta(days=15),
             df_var['fecha'].max() + pd.Timedelta(days=15))

fig.text(0, -0.02, FUENTE, fontsize=9, color='gray')
plt.tight_layout()
plt.savefig("C:/Users/drankin/Documents/Base_limpia_auto_energía/grafica_variacion.png",
            dpi=300, bbox_inches='tight')
plt.show()

print("Graficas guardadas.")