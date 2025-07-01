import asyncio
import websockets

async def test_websocket_echo(uri):
    try:
        async with websockets.connect(uri) as websocket:
            test_message = "hello from python"
            await websocket.send(test_message)
            print(f"Sent: {test_message}")
            response = await websocket.recv()
            print(f"Received: {response}")
            if response == test_message:
                print("WebSocket echo test: SUCCESS")
            else:
                print("WebSocket echo test: FAIL (response mismatch)")
    except Exception as e:
        print(f"WebSocket connection failed: {e}")

if __name__ == "__main__":
    # Connect to the Java WebSocket server directly (bypassing nginx)
    asyncio.run(test_websocket_echo("ws://localhost:8080/ws"))
