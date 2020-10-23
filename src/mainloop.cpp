
void Run( Array<String> const& argsList, MemoryArena* globalArena, MemoryArena* tmpArena )
{
    // Parse arguments
    if( argsList.count == 0 )
    {
        globalPlatform.Error( "Need an input file" );
        return;
    }

    String inputFilename = argsList[0];
    char const* filename_str = inputFilename.CString( tmpArena );
    // TODO Temp memory scopes
    Buffer readResult = globalPlatform.ReadEntireFile( filename_str, tmpArena );

    Lexer::Scan( String( readResult ), filename_str );
}
