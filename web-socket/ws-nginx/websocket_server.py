import asyncio
import websockets
import os
from urllib.parse import urlparse, parse_qs

async def echo_handler(websocket, size_kb):
    async for message in websocket:
        await websocket.send(message)

async def push_burst_handler(websocket, bursts, size_kb):
    payload = os.urandom(size_kb * 1024)
    for _ in range(bursts):
        await websocket.send(payload)
        await asyncio.sleep(0)  # Yield to event loop

async def push_stream_handler(websocket, rate, duration, size_kb):
    payload = os.urandom(size_kb * 1024)
    end_time = asyncio.get_event_loop().time() + duration
    while asyncio.get_event_loop().time() < end_time:
        await websocket.send(payload)
        await asyncio.sleep(1.0 / rate)

async def handler(websocket):
    # Parse query parameters from websocket.path
    parsed = urlparse(websocket.path)
    params = parse_qs(parsed.query)
    mode = params.get('mode', ['echo'])[0]
    size_kb = int(params.get('size_kb', [64])[0])
    if mode == 'echo':
        await echo_handler(websocket, size_kb)
    elif mode == 'push_burst':
        bursts = int(params.get('bursts', [20])[0])
        await push_burst_handler(websocket, bursts, size_kb)
    elif mode == 'push_stream':
        rate = int(params.get('rate', [10])[0])
        duration = int(params.get('duration', [30])[0])
        await push_stream_handler(websocket, rate, duration, size_kb)
    else:
        await websocket.close(code=4000, reason="Unknown mode")

async def main():
    async with websockets.serve(handler, '0.0.0.0', 8765, max_size=None):
        await asyncio.Future()  # Run forever

if __name__ == '__main__':
    asyncio.run(main()) 