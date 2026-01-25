#pragma once

#include "Charactor.hpp"
#include <cstdint>
#include <cstddef>
#include <cstring>
#include <new>

namespace Puma {
namespace Types
{
#pragma pack(push, 1)
    union String
    {
    public:
        // Lifetime
        String() noexcept;
        String(const String& source) noexcept;
        String(const char* data, std::uint32_t byteLength) noexcept;
        ~String() noexcept;

        String& operator=(const String& source) noexcept;
        String ToString() const noexcept;

        // Public API
		// get str length - length in characters (code points)
		std::uint32_t Length() const noexcept;
		// get str size - length in bytes
        std::uint32_t StrSize() const noexcept;
		// get variable size - number of bytes used to store the variable
        std::uint32_t VarSize() const noexcept;

		// iterator range support
		const char* BeginConst() const noexcept;
        const char* EndConst() const noexcept;
        const char* NextConst() const noexcept;
        const char* PreviousConst() const noexcept;

    private:
        // Layout (private)
        struct { std::uint8_t tag; char codeUnits[15]; } shortStr;

    #if INTPTR_MAX == INT64_MAX
        struct { std::uint8_t tag; std::uint8_t reserved[3]; std::uint32_t strSize; const char* ptr; } longStr;
    #elif INTPTR_MAX == INT32_MAX
        struct { std::uint8_t tag; std::uint8_t reserved[3]; std::uint32_t strSize; std::uint32_t reserved2; const char* ptr; } longStr;
    #else
    #error Unsupported pointer size
    #endif
		// copy or zero the union
        struct { std::uint64_t firstHalf; std::uint64_t secondHalf; } packedValues;

        // Masks (private) - UPPER_CASE
        static constexpr std::uint8_t SHORT_MASK  = 0x80;
        static constexpr std::uint8_t LENGTH_MASK = 0x0F;
        static constexpr std::uint8_t LONG_MASK   = 0x80;

        // get pointer to string codeUnits
        const char* codeUnits() const noexcept;

        // Helpers (private) - lowerCamelCase
        bool isShort() const noexcept;
        bool isLong()  const noexcept;

        void release() noexcept;

        void fromString(const String& source) noexcept;

        // iterator state
        mutable const char* m_currentConst;
    };
#pragma pack(pop)

} // namespace Types
} // namespace Puma