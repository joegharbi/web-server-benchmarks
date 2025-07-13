import asyncio
import websockets

async def test():
    try:
        async with websockets.connect('ws://localhost:8003/ws') as ws:
            await ws.send(b'hello')
            resp = await ws.recv()
            print('Received:', resp)
    except Exception as e:
        print('WebSocket error:', e)

if __name__ == '__main__':
    asyncio.run(test()) 