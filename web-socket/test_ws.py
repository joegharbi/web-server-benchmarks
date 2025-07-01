# import asyncio
# import websockets

# async def test():
#     try:
#         async with websockets.connect("ws://localhost:8001/ws") as ws:
#             await ws.send("Hello")
#             response = await ws.recv()
#             print(f"Received: {response}")
#     except Exception as e:
#         print(f"Error: {e}")

# asyncio.run(test())


# import asyncio
# import websockets

# async def test():
#     try:
#         async with websockets.connect("ws://localhost:8001/ws") as ws:
#             # 1MB payload
#             payload = "x" * (1024 * 1024)  # 1MB of 'x'
#             print("Sending 1MB payload...")
#             await ws.send(payload)
#             response = await ws.recv()
#             print(f"Received: {len(response)} bytes")
#             if response == payload:
#                 print("Echo matches!")
#             else:
#                 print("Echo mismatch!")
#     except Exception as e:
#         print(f"Error: {e}")

# asyncio.run(test())

import asyncio
import websockets
import time

async def test():
    try:
        async with websockets.connect("ws://localhost:8080/ws") as ws:
            payload = "x" * (1024 * 1024)  # 1MB
            start_time = time.time()
            while time.time() - start_time < 10:  # 10 seconds
                print("Sending 1MB...")
                await ws.send(payload)
                response = await ws.recv()
                print(f"Received: {len(response)} bytes")
                await asyncio.sleep(0.1)  # ~10 messages/s
    except Exception as e:
        print(f"Error: {e}")

asyncio.run(test())