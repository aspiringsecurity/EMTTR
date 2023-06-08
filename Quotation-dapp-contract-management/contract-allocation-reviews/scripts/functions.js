const player = args[0];
const hasWon = args[1];
const playersRequest = Functions.makeHttpRequest({ url: `https://api.chess.com/pub/leaderboards`
});
const playersResponse = await Promise.resolve(playersRequest);

if (!playersResponse.error) {
    const player1 = playersResponse.data.daily[0].username;
    const player2 = playersResponse.data.daily[3].username;

    const player1Request = Functions.makeHttpRequest({ url: `https://api.chess.com/pub/player/${player1}/stats`
    });

    const player2Request = Functions.makeHttpRequest({ url: `https://api.chess.com/pub/player/${player2}/stats`
    });

    const player1Response = await Promise.resolve(player1Request);
    const player2Response = await Promise.resolve(player2Request);

    let player1Rating, player2Rating;

    if (!player1Response.error) {
        player1Rating = player1Response.data.chess_daily.last.rating;
        if (!player2Response.error) {
            player2Rating = player2Response.data.chess_daily.last.rating;
        } else {
              return Functions.ensodeString(player2Response.error);
         } 
    } else {
      return Functions.ensodeString(player1Response.error);
    } 
    
    const E1 = 1 / (1 + 10 ** ((player2Rating - player1Rating)/400));
    const E2 = 1 - E1;

    const E = [E1, E2];

    const k = 32;

    const Rating_new = (R, E, win) => {
        return (
            R + 32 * (win - E)
        )
    };

    let ratings = [player1Rating, player2Rating];

    const newRatings = Rating_new(ratings[player], E[player], hasWon);
    return Functions.encodeUint256(Math.round(newRatings));
    
  } else {
  return Functions.encodeString(playersResponse.error);
}