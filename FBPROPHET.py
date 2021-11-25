import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from fbprophet import Prophet
from sklearn.metrics import mean_squared_error as mse
#SAME ALGORITM WAS REPEATED FOR FIRS AND SECOND WAVE CASES
df = pd.read_csv('../examples/exam.csv', ';')
df.head()
#np.shape(df)
#list(df)
m = Prophet()
m.fit(df)
future = m.make_future_dataframe(periods=180)
future.head(50)
future.tail(50)
forecast = m.predict(future)
forecast[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].head(50)
#np.shape(forecast)
forecast = m.predict(future)
forecast[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].tail(50)
#MEAN SQUARE ERROR FOR SECOND WAVE DEATH
a = pd.DataFrame(df)
y_true = a.y
b= pd.DataFrame(forecast)
y_pred = b.yhat[1:101]

mse(y_true, y_pred)
fig1 = m.plot(forecast, xlabel = 'Date', ylabel = 'Second_Wave_Death')
fig2 = m.plot_components(forecast)
df = pd.read_csv('../examples/r.csv', ';')
df.head()
#np.shape(df)
m = Prophet()
m.fit(df)
future = m.make_future_dataframe(periods=180)
future.tail(50)
future.head(50)
forecast = m.predict(future)
forecast[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].tail(50)
forecast = m.predict(future)
forecast[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].head(50)
#MEAN SQUARE ERROR FOR First WAVE DEATH
a = pd.DataFrame(df)
y_true = a.y
b= pd.DataFrame(forecast)
y_pred = b.yhat[1:101]

mse(y_true, y_pred)
fig1 = m.plot(forecast, xlabel ='Date', ylabel = 'First_Wave_Death')
