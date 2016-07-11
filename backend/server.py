import asyncio
import json

import aiohttp
from aiohttp import web


async def websocket_handler(request):
    ws = web.WebSocketResponse()
    await ws.prepare(request)

    request.app['players'][ws] = []

    async for msg in ws:
        if msg.tp == aiohttp.MsgType.text:
            request.app['players'][ws] = json.loads(msg.data)  # cart

            message = json.dumps(list(request.app['players'].values()))  # list of carts
            for _ws in request.app['players']:
                _ws.send_str(message)

        elif msg.tp == aiohttp.MsgType.error:
            print('ws connection closed with exception %s' %
                  ws.exception())

    del request.app['players'][ws]

    return ws

loop = asyncio.get_event_loop()
app = web.Application(loop=loop)
app['players'] = dict()
app.router.add_route('GET', '/', websocket_handler)
web.run_app(app, port=8765)
