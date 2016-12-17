import asyncio
import json

import aiohttp
from aiohttp import web


async def send_updates(request):
    message = json.dumps(list(request.app['customers'].values()))  # list of carts
    for _ws in request.app['customers']:
        _ws.send_str(message)


async def websocket_handler(request):
    ws = web.WebSocketResponse()
    await ws.prepare(request)
    request.app['customers'][ws] = []
    await send_updates(request)

    async for msg in ws:
        if msg.tp == aiohttp.MsgType.text:
            request.app['customers'][ws] = json.loads(msg.data)  # cart
            print("Somebody shopping")
            await send_updates(request)
        elif msg.tp == aiohttp.MsgType.error:
            print('ws connection closed with exception %s' %
                  ws.exception())

    del request.app['customers'][ws]
    return ws


loop = asyncio.get_event_loop()
app = web.Application(loop=loop)
app['customers'] = dict()
app.router.add_route('GET', '/', websocket_handler)
web.run_app(app, port=8765)
