from flask import Flask
import datetime

app = Flask(__name__)

@app.route('/')
def hello():
    current_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    return f"<h1>Hello, Energy Test!</h1><p>Current time: {current_time}</p>"

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8000, debug=False)