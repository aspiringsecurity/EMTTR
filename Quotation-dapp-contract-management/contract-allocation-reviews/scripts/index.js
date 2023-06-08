const main = async () => {
  const res = await fetch('https://api.chess.com/pub/leaderboards');
if (res.ok) {
  const data = await res.json();
  player1 = data.daily[0].username;
  player2 = data.daily[3].username;
  //console.log(player2);

  const resPlayer1 = await fetch(`https://api.chess.com/pub/player/${player1}/stats`);
  const resPlayer2 = await fetch(`https://api.chess.com/pub/player/${player2}/stats`);
  let player1Rating, player2Rating;

  if (resPlayer1.ok) {
    const player1Data = await resPlayer1.json();
    player1Rating = player1Data.chess_daily.last.rating;
    console.log(player1Rating);
  }

  if (resPlayer2.ok) {
    const player2Data = await resPlayer2.json();
    player2Rating = player2Data.chess_daily.last.rating;
    console.log(player2Rating);
  }

  if (player1Rating && player2Rating) {
    const E2 = 1 / (1 + 10 ** ((player1Rating - player2Rating)/400));
    const E1 = 1 / (1 + 10 ** ((player2Rating - player1Rating)/400))

    const E = [E1, E2];

    const k = 32;

    const Rating_new = (R, E, win) => {
        return (
            R + 32 * (win - E)
        )
    };

    let ratings = [player1Rating, player2Rating];
    console.log(Math.round(Rating_new(ratings[0], E[0],true)));
  }
}
}

main();