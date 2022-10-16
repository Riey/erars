import { ErarsContext, ErarsReturn, init_logger } from "../pkg/erars";

const game = fetch("./game.era").then(g => g.arrayBuffer());

init_logger();

async function start() {
  const config = await fetch("./emuera.config").then(g => g.text());
  const ctx = new ErarsContext(new Uint8Array(await game), config);
  console.log("Load success!");

  const ret = ctx.run();

  if (ret == ErarsReturn.Exit) {
    console.log("Return: Exit");
  } else if (ret == ErarsReturn.Input) {
    console.log("Return: Input");
  } else if (ret == ErarsReturn.InputTimeout) {
    console.log("Return: InputTimeout");
  } else if (ret == ErarsReturn.Redraw) {
    console.log("Return: Redraw");
  } else {
    console.log("Return: Unknown");
  }

  const display = ctx.display(0);
  console.log("console display: ", display);
}

start();
