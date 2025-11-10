import asyncio
import websockets
import os

async def echo_handler(websocket):
    async for message in websocket:
        await websocket.send(message)

async def handler(websocket):
    # Simple echo handler for benchmarking
    await echo_handler(websocket)

async def main():
    async with websockets.serve(handler, '0.0.0.0', 8765, max_size=None):
        await asyncio.Future()  # Run forever

if __name__ == '__main__':
    asyncio.run(main()) 