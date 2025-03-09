from flask import Flask
import datetime

app = Flask(__name__)

@app.route('/')
def hello():
    current_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    return f"<h1>Hello, Energy Test!</h1><p>Current time: {current_time}</p>"