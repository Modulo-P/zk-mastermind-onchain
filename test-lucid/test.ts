import { Lucid, Emulator } from "https://deno.land/x/lucid/mod.ts"


const emulator = new Emulator([{
    address: "addr_test...",
    assets: { lovelace: 3000000000n },
  }]);
  
const lucid = await Lucid.new(emulator);

/*
const snarkScript = {
    type: "PlutusV2",
    script: "59099a590997010000...",
  };

const snarkScriptAddress = lucid.utils.validatorToAddress(
  snarkScript,
);
*/